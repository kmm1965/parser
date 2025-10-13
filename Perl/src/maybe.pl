package Maybe;

use strict;
use warnings;

use overload '""' => \&to_string;

use overload
  '==' => \&equals,
  '|' => \&orElse,
  fallback => 1; # Allows fallback to default behavior for unhandled ops

sub Just {
  my ($class, $value) = @_;
  bless { value => $value, type => 'Just' }, $class;
}

sub Nothing {
  bless { type => 'Nothing' }, shift;
}

sub equals {
  my ($self, $other) = @_;
  # Check if $other is also an object of the same class
  0 unless ref $other eq ref $self;
  $self->isJust && $other->isJust ?
    $self->{value} == $other->{value} :
    $self->isNothing && $other->isNothing
}

sub to_string {
  my $self = shift;
  $self->isJust ? "Just($self->{value})" : "Nothing"
}

sub isJust {
  shift->{type} eq 'Just'
}

sub isNothing {
  not shift->isJust
}

sub fmap {
  my ($self, $func) = @_;
  $self->andThen(sub { Maybe->Just($func->(shift)) })
}

sub andThen {
  my ($self, $func) = @_;
  $self->isJust ?
    $func->($self->{value}) :
    Maybe->Nothing
}

sub orElse {
  my ($self, $func) = @_;
  $self->isJust ? $self : $func->()
}

sub fromMaybe {
  my ($self, $default_value) = @_;
  $self->isJust ? $self->{value} : $default_value
}

1