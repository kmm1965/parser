require "./src/calculator.pl";

use strict;
use warnings;
use Test::More; #tests => 2; # Declare the number of expected tests
use Math::Trig;

sub calc {
 Calculator->calculate(shift);
}

sub test_calculator {
  is(calc("72 - 7 - (1 - 2) * 3"), Maybe->Just(Pair->new(68.0, "")),
    "calc(\"72 - 7 - (1 - 2) * 3\") should be equal to Just((68.0, \"\"))");
  is(calc("-72 - 7 - (1 - 2) * 3"), Maybe->Just(Pair->new(-76.0, "")),
    "calc(\"-72 - 7 - (1 - 2) * 3\") should be equal to Just((-76.0, \"\"))");
  is(calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"), Maybe->Just(Pair->new(-8.889, "")),
    "calc(\" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)\") should be equal to Just((-8.889, \"\"))");
  is(calc("3^(1+1)^3")->fmap(sub {
      my $pair = shift; Pair->new(int($pair->{first} + 0.5), $pair->{second});
    }), Maybe->Just(Pair->new(6561, "")),
    "calc(\"3^(1+1)^3\") should be equal to Just((6561, \"\"))");
  is(calc("sin(1+1)"), Maybe->Just(Pair->new(sin(2.0), "")),
    "calc(\"sin(1+1)\") should be equal to Just((sin(2.0), \"\"))");
  is(calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"), Maybe->Just(Pair->new(-0.3634085731426532, "")),
    "calc(\"sin ( 2_SQRTPI * sqr ( 2 ) - 1 )\") should be equal to Just(-0.3634085731426532, \"\"))");
  is(calc("sqr(2 + 3)"), Maybe->Just(Pair->new(25, "")),
    "calc(\"sqr(2 + 3)\") should be equal to Just(25, \"\"))");
  is(calc("sin(-PI/4)"), Maybe->Just(Pair->new(sin(-pi/4), "")),
    "calc(\"sin(-PI/4)\") should be equal to Just(sin(-pi/4), \"\"))");
  is(calc("E"), Maybe->Just(Pair->new(2.7182818284590452, "")),
    "calc(\"E\") should be equal to Just(2.7182818284590452, \"\"))");
  is(calc(" E ^ PI"), Maybe->Just(Pair->new(23.140692632779267, "")),
    "calc(\" E ^ PI\") should be equal to Just(23.140692632779267, \"\"))");
  is(calc(" PI ^ E"), Maybe->Just(Pair->new(22.45915771836104, "")),
    "calc(\" PI ^ E\") should be equal to Just(22.45915771836104, \"\"))");
  is(calc("LN10"), Maybe->Just(Pair->new(2.3025850929940457, "")),
    "calc(\"LN10\") should be equal to Just(2.3025850929940457, \"\"))");
}

test_calculator;
done_testing;
