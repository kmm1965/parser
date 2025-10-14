package Parser;

require "./src/maybe.pl";
require "./src/pair.pl";

use strict;
use warnings;

use overload
  '|' => \&orElse,
  '>>' => \&skip,
  '~' => \&optional_s,
  fallback => 1; # Allows fallback to default behavior for unhandled ops

sub new {
  my ($class, $unp) = @_;
  bless { unp => $unp, type => 'Parser' }, $class
}

sub parse {
  my ($self, $inp) = @_;
  $self->{unp}($inp)
}

sub fmap {
  my ($self, $func) = @_;
  $self->andThen(sub { Parser->pure($func->(shift)) } )
}

sub andThen {
  my ($self, $func) = @_;
  Parser->new(sub {
    $self->parse(shift)->andThen(sub {
      my $pair = shift;
      $func->($pair->{first})->parse($pair->{second})
    })
  })
}

sub skip {
  my ($self, $fp) = @_;
  $self->andThen(sub { $fp->() })
}

sub orElse {
  my ($self, $fp) = @_;
  Parser->new(sub {
    my $inp = shift;
    $self->parse($inp)->orElse(sub {
        $fp->()->parse($inp)
    })
  })
}

sub orElseVal {
  my ($self, $other) = @_;
  $self->orElse(sub { $other })
}

sub apply {
  my ($self, $fp) = @_;
  $self->andThen(sub {
    $fp->()->fmap(shift)
  })
}

sub pure {
  my ($class, $x) = @_;
  Parser->new(sub {
    Maybe->Just(Pair->new($x, shift))
  })
}

sub empty {
  Parser->new(sub { Maybe->Nothing })
}

sub empty_string {
  Parser->pure("")
}

sub some {
  my $self = shift;
  $self->fmap(sub {
    my $c = shift;
    sub { $c . shift }
  })->apply(sub { $self->many })
}

sub many {
  shift->some->orElse(sub { empty_string })
}

sub any_char {
  Parser->new(sub {
    my $inp = shift;
    length($inp) > 0 ? Maybe->Just(Pair->new(substr($inp, 0, 1), substr($inp, 1))) : Maybe->Nothing
  })
}

sub satisfy {
  my ($class, $pred) = @_;
  any_char->andThen(sub {
    my $c = shift;
    $pred->($c) ? Parser->pure($c) : Parser->empty
  })
}

sub spaces {
  Parser->satisfy(sub {
    shift =~ /^\s*$/
  })->many
}
  
sub between {
  my ($class, $open, $close, $fp) = @_;
  $open >> sub {
    $fp->()->andThen(sub {
      my $x = shift;
      $close >> sub { Parser->pure($x) }
    })
  }
}

sub token {
  my $self = shift;
  Parser->between(spaces, spaces, sub { $self })
}

sub optional_s {
  shift | sub { empty_string }
}

sub rest {
  my ($fp, $ff, $op, $x) = @_;
  $op->andThen(sub {
    my $f = shift;
    $fp->()->andThen(sub {
      $ff->($f->($x, shift))
    })
  }) | sub { Parser->pure($x) }
}

sub rest_l {
  my ($self, $op, $x) = @_;
  rest(sub { $self }, sub { $self->rest_l($op, shift) }, $op, $x)
}

sub chainl1 {
  my ($self, $op, $negate_first) = @_;
  $self->andThen(sub { my $x = shift; $self->rest_l($op, $negate_first ? -$x : $x) })
}

sub rest_r {
  my ($self, $op, $x) = @_;
  rest(sub { $self->chainr1($op) }, sub { Parser->pure(shift) }, $op, $x)
}

sub chainr1 {
  my ($self, $op) = @_;
  $self->andThen(sub { $self->rest_r($op, shift) })
}
