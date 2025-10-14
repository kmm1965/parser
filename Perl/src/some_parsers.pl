package SomeParsers;

require "./src/parser.pl";

#use strict;
use warnings;

sub char {
  my ($class, $c) = @_;
  Parser->satisfy(sub { shift eq $c })
}

sub symbol {
  my ($class, $c) = @_;
  SomeParsers->char($c)->token
}

sub alnum {
  Parser->satisfy(sub { shift =~ /\w/ })
}

sub name {
  my ($class, $n) = @_;
  alnum->some->andThen(sub {
    (shift eq $n ? Parser->pure($n) : Parser->empty)->token
  })
}

sub digit {
  Parser->satisfy(sub { shift =~ /^\d$/ })
}

sub digits {
  digit->many
}

sub sign {
  ~(SomeParsers->char('+') | sub { SomeParsers->char('-') })
}

sub usign {
  ~(SomeParsers->symbol('+') | sub { SomeParsers->symbol('-') })
}

sub double {
  (digits->andThen(
    sub { my $int_part = shift; (~(SomeParsers->char('.') >> sub { digits }))->andThen(
    sub { my $frac_part = shift; (~((SomeParsers->char('e') | sub { SomeParsers->char('E') }) >> sub { sign })->andThen(
      sub { my $exp_sign = shift; digit->some->andThen(
      sub { my $exp_digits = shift; Parser->pure($exp_sign . $exp_digits) }) }))->andThen(
    sub { my $exp_part = shift; length($int_part) > 0 || length($frac_part) > 0 ?
      Parser->pure(0 + ($int_part .
        (length($frac_part) > 0 ? '.' . $frac_part : "") .
        (length($exp_part) > 0 ? 'e' . $exp_part : ""))) :
      Parser->empty }) }) }))->token
}
