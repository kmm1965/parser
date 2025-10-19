require "./src/calculator.pl";

use strict;
use warnings;
use Test::More; #tests => 2; # Declare the number of expected tests
use Math::Trig;

sub calc {
 Calculator->calculate(shift);
}

sub test_funcs {
  is(calc("sin(2.0)"), Maybe->Just(Pair->new(sin(2.0), "")),
    "calc(\"sin(2.0)\") should be equal to Just((sin(2.0), \"\"))");
  is(calc("cos(2.0)"), Maybe->Just(Pair->new(cos(2.0), "")),
    "calc(\"cos(2.0)\") should be equal to Just((cos(2.0), \"\"))");
  is(calc("asin(0.5)"), Maybe->Just(Pair->new(asin(0.5), "")),
    "calc(\"asin(0.5)\") should be equal to Just((asin(0.5), \"\"))");
  is(calc("acos(0.5)"), Maybe->Just(Pair->new(acos(0.5), "")),
    "calc(\"acos(0.5)\") should be equal to Just((acos(0.5), \"\"))");
  is(calc("sinh(2.0)"), Maybe->Just(Pair->new(sinh(2.0), "")),
    "calc(\"sinh(2.0)\") should be equal to Just((sinh(2.0), \"\"))");
  is(calc("cosh(2.0)"), Maybe->Just(Pair->new(cosh(2.0), "")),
    "calc(\"cosh(2.0)\") should be equal to Just((cosh(2.0), \"\"))");
  is(calc("asinh(2.0)"), Maybe->Just(Pair->new(asinh(2.0), "")),
    "calc(\"asinh(2.0)\") should be equal to Just((asinh(2.0), \"\"))");
  is(calc("acosh(2.0)"), Maybe->Just(Pair->new(acosh(2.0), "")),
    "calc(\"acosh(2.0)\") should be equal to Just((acosh(2.0), \"\"))");
  is(calc("tan(2.0)"), Maybe->Just(Pair->new(tan(2.0), "")),
    "calc(\"tan(2.0)\") should be equal to Just((tan(2.0), \"\"))");
  is(calc("log(2.0)"), Maybe->Just(Pair->new(log(2.0), "")),
    "calc(\"log(2.0)\") should be equal to Just((log(2.0), \"\"))");
  is(calc("log10(2.0)"), Maybe->Just(Pair->new(log(2.0) / log(10.0), "")),
    "calc(\"log10(2.0)\") should be equal to Just((log(2.0) / log(10.0), \"\"))");
  is(calc("exp(2.0)"), Maybe->Just(Pair->new(exp(2.0), "")),
    "calc(\"exp(2.0)\") should be equal to Just((exp(2.0), \"\"))");
  is(calc("sqrt(2.0)"), Maybe->Just(Pair->new(sqrt(2.0), "")),
    "calc(\"sqrt(2.0)\") should be equal to Just((sqrt(2.0), \"\"))");
  is(calc("sqr(2.0)"), Maybe->Just(Pair->new(4.0, "")),
    "calc(\"sqr(2.0)\") should be equal to Just((4.0, \"\"))");
}

sub test_consts {
  is(calc("E"), Maybe->Just(Pair->new(2.71828182845904523536, "")),
    "calc(\"E\") should be equal to Just((2.71828182845904523536, \"\"))");
  is(calc("LOG2E"), Maybe->Just(Pair->new(1 / log(2.0), "")),
    "calc(\"LOG2E\") should be equal to Just((1 / log(2.0), \"\"))");
  is(calc("LOG10E"), Maybe->Just(Pair->new(0.434294481903251827651, "")),
    "calc(\"LOG10E\") should be equal to Just((0.434294481903251827651, \"\"))");
  is(calc("LOG10E"), Maybe->Just(Pair->new(1 / log(10.0), "")),
    "calc(\"LOG10E\") should be equal to Just((1 / log(10.0), \"\"))");
  is(calc("LN2"), Maybe->Just(Pair->new(log(2.0), "")),
    "calc(\"LN2\") should be equal to Just((log(2.0), \"\"))");
  is(calc("LN10"), Maybe->Just(Pair->new(log(10.0), "")),
    "calc(\"LN10\") should be equal to Just((log(10.0), \"\"))");
  is(calc("PI"), Maybe->Just(Pair->new(pi, "")),
    "calc(\"PI\") should be equal to Just((pi, \"\"))");
  is(calc("PI_2"), Maybe->Just(Pair->new(pi / 2, "")),
    "calc(\"PI_2\") should be equal to Just((pi / 2, \"\"))");
  is(calc("PI_4"), Maybe->Just(Pair->new(pi / 4, "")),
    "calc(\"PI_4\") should be equal to Just((pi / 4, \"\"))");
  is(calc("1_PI"), Maybe->Just(Pair->new(1 / pi, "")),
    "calc(\"1_PI\") should be equal to Just((1 / pi, \"\"))");
  is(calc("2_PI"), Maybe->Just(Pair->new(2 / pi, "")),
    "calc(\"2_PI\") should be equal to Just((2 / pi, \"\"))");
  is(calc("2_SQRTPI"), Maybe->Just(Pair->new(2 / sqrt(pi), "")),
    "calc(\"2_SQRTPI\") should be equal to Just((2 / sqrt(pi), \"\"))");
  is(calc("SQRT2"), Maybe->Just(Pair->new(sqrt(2.0), "")),
    "calc(\"SQRT2\") should be equal to Just((sqrt(2.0), \"\"))");
  is(calc("SQRT1_2"), Maybe->Just(Pair->new(sqrt(0.5), "")),
    "calc(\"SQRT1_2\") should be equal to Just((1 / sqrt(2.0), \"\"))");
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

test_funcs;
test_consts;
test_calculator;
done_testing;
