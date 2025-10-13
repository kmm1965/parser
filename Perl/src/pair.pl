package Pair;

use strict;
use warnings;

use overload '""' => \&to_string;

use overload
  '==' => \&equals,
  fallback => 1; # Allows fallback to default behavior for unhandled ops

sub new {
  my ($class, $first, $second) = @_;
  bless { first => $first, second => $second, type => 'Pair' }, $class;
}

sub equals {
  my ($self, $other) = @_;
  # Check if $other is also an object of the same class
  0 unless ref $other eq ref $self;
  $self->{first} == $other->{first} &&
    $self->{second} == $other->{second};
}

sub to_string {
  my $self = shift;
  "Pair($self->{first},$self->{second})";
}

1;
