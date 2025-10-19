use "../src/calculator.sml";
use "test_core.sml";

open Calculator
open Maybe
open Testing

fun test_funcs() =
  print "Testing funcs...\n";

  ptestr(calculate("sin(2.0)"), Just((Math.sin(2.0), "")));
  ptestr(calculate("cos(2.0)"), Just((Math.cos(2.0), "")));
  ptestr(calculate("asin(0.5)"), Just((Math.asin(0.5), "")));
  ptestr(calculate("acos(0.5)"), Just((Math.acos(0.5), "")));
  ptestr(calculate("sinh(2.0)"), Just((Math.sinh(2.0), "")));
  ptestr(calculate("cosh(2.0)"), Just((Math.cosh(2.0), "")));
  ptestr(calculate("tan(2.0)"), Just((Math.tan(2.0), "")));
  ptestr(calculate("log(2.0)"), Just((Math.ln(2.0), "")));
  ptestr(calculate("log10(2.0)"), Just((Math.log10(2.0), "")));
  ptestr(calculate("exp(2.0)"), Just((Math.exp(2.0), "")));
  ptestr(calculate("sqrt(2.0)"), Just((Math.sqrt(2.0), "")));
  ptestr(calculate("sqr(2.0)"), Just((4.0, "")));

fun test_consts() =
  print "Testing consts...\n";

  val pi = 3.14159265358979323846;
  ptestr(calculate("E"), Just((2.71828182845904523536, "")));
  ptestr(calculate("LOG2E"), Just((1.0 / Math.ln(2.0), "")));
  ptestr(calculate("LOG10E"), Just((0.4342944819032518, "")));
  ptestr(calculate("LOG10E"), Just((1.0 / Math.ln(10.0), "")));
  ptestr(calculate("LN2"), Just((Math.ln(2.0), "")));
  ptestr(calculate("LN10"), Just((Math.ln(10.0), "")));
  ptestr(calculate("PI"), Just((pi, "")));
  ptestr(calculate("PI_2"), Just((pi / 2.0, "")));
  ptestr(calculate("PI_4"), Just((pi / 4.0, "")));
  ptestr(calculate("1_PI"), Just((1.0 / pi, "")));
  ptestr(calculate("2_PI"), Just((2.0 / pi, "")));
  ptestr(calculate("2_SQRTPI"), Just((2.0 / Math.sqrt(pi), "")));
  ptestr(calculate("SQRT2"), Just((Math.sqrt(2.0), "")));
  ptestr(calculate("SQRT1_2"), Just((Math.sqrt(0.5), "")));

fun testCalculator() =
  ptestr(calculate("72 - 7 - (1 - 2) * 3"), Just((68.0, "")));
  ptestr(calculate("-72 - 7 - (1 - 2) * 3"), Just((~76.0, "")));
  ptestr(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"), Just((~8.889, "")));
  ptestr(map(calculate("3^(1+1)^3"), fn (p) => (Real.fromInt(Real.floor(#1 p + 0.5)), #2 p)), Just((6561.0, "")));
  ptestr(calculate("sin(1+1)"), Just((Math.sin(2.0), "")));
  ptestr(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"), Just((~0.3634085731426532, "")));
  ptestr(calculate("sqr(2 + 3)"), Just((25.0, "")));
  ptestr(calculate("sin(-PI/4)"), Just((Math.sin(~Math.pi/4.0), "")));
  ptestr(calculate(" E ^ PI"), Just((23.140692632779267, "")));
  ptestr(calculate(" PI ^ E"), Just((22.45915771836104, "")));

test_funcs();
test_consts();
testCalculator();
