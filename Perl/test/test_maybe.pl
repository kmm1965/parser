require "./src/maybe.pl";

use strict;
use warnings;
use Test::More; #tests => 2; # Declare the number of expected tests

sub test_maybe_map {
  my $fsin = sub { my ($x) = @_; sin($x) };
  my $toString = sub { my ($x) = @_; "$x" };

  is(Maybe->Just(1.0)->fmap($fsin), Maybe->Just(sin(1.0)), "Just(1.0)->fmap(sin) should be equal to Just(sin(1.0))");
  is(Maybe->Nothing->fmap($fsin), Maybe->Nothing, "Nothing->fmap(sin) should be equal to Nothing");
  is(Maybe->Just(1)->fmap($toString), Maybe->Just("1"), "Just(1)->fmap(toString) should be equal to Just(\"1\")");
  is(Maybe->Nothing->fmap($toString), Maybe->Nothing, "Nothing->fmap(toString) should be equal to Nothing");
}

sub test_maybe_andThen {
  my $safe_sqrt = sub { my ($x) = @_; $x >= 0 ? Maybe->Just(sqrt($x)) : Maybe->Nothing };
  my $safe_log = sub { my ($x) = @_; $x > 0 ? Maybe->Just(log($x)) : Maybe->Nothing };
  my $toString = sub { my ($i) = @_; $i % 2 == 0 ? Maybe->Just("$i") : Maybe->Nothing };

  is($safe_sqrt->(2.0), Maybe->Just(sqrt(2.0)), "safe_sqrt(2.0) should be equal to Just(sqrt(2.0))");
  is($safe_sqrt->(0.0), Maybe->Just(0.0), "safe_sqrt(0.0) should be equal to Just(0.0)");
  is($safe_sqrt->(-2.0), Maybe->Nothing, "safe_sqrt(-2.0) should be equal to Nothing");

  is($safe_log->(2.0), Maybe->Just(log(2.0)), "safe_log(2.0) should be equal to Just(log(2.0))");
  is($safe_log->(0.0), Maybe->Nothing, "safe_log(0.0) should be equal to Nothing");
  is($safe_log->(-2.0), Maybe->Nothing, "safe_log(-2.0) should be equal to Nothing");

  is(Maybe->Just(2.0)->andThen($safe_sqrt), Maybe->Just(sqrt(2.0)),
    "Just(2.0)->andThen(safe_sqrt) should be equal to Just(sqrt(2.0))");
  is(Maybe->Just(0.0)->andThen($safe_sqrt), Maybe->Just(0.0),
    "Just(0.0)->andThen(safe_sqrt) should be equal to Just(0.0)");
  is(Maybe->Just(-2.0)->andThen($safe_sqrt), Maybe->Nothing,
    "Just(-2.0)->andThen(safe_sqrt) should be equal to Nothing");

  is(Maybe->Just(2.0)->andThen($safe_sqrt)->andThen($safe_log), Maybe->Just(log(sqrt(2.0))),
    "Just(2.0)->andThen(safe_sqrt)->andThen(safe_log) should be equal to Just(log(sqrt(2.0)))");
  is(Maybe->Just(0.0)->andThen($safe_sqrt)->andThen($safe_log), Maybe->Nothing,
    "Just(0.0)->andThen(safe_sqrt)->andThen(safe_log) should be equal to Nothing");
  is(Maybe->Just(-2.0)->andThen($safe_sqrt)->andThen($safe_log), Maybe->Nothing,
    "Just(-2.0)->andThen(safe_sqrt)->andThen(safe_log) should be equal to Nothing");

  is(Maybe->Just(2)->andThen($toString), Maybe->Just("2"),
    "Just(2)->andThen(toString) should be equal to Just(\"2\")");
  is(Maybe->Just(1)->andThen($toString), Maybe->Nothing,
    "Just(1)->andThen(toString) should be equal to Nothing");
  is(Maybe->Nothing->andThen($toString), Maybe->Nothing,
    "Nothing->andThen(toString) should be equal to Nothing");
}

sub test_maybe_orElse {
  my $f2 = sub { Maybe->Just(2.0) };

  is(Maybe->Just(1.0)->orElse($f2), Maybe->Just(1.0),
    "Just(1.0)->orElse(Just(2.0)) should be equal to Just(1.0)");
  is(Maybe->Nothing->orElse($f2), Maybe->Just(2.0),
    "Nothing->orElse(Just(2.0)) should be equal to Just(2.0)");

  is(Maybe->Just(1.0) | $f2, Maybe->Just(1.0),
    "Just(1.0) | Just(2.0) should be equal to Just(1.0)");
  is(Maybe->Nothing | $f2, Maybe->Just(2.0),
    "Nothing | Just(2.0) should be equal to Just(2.0)");
}

test_maybe_map;
test_maybe_andThen;
test_maybe_orElse;
done_testing;
