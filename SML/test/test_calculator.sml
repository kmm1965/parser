use "../src/calculator.sml";
use "test_core.sml";

open Calculator
open Maybe
open Testing

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

testCalculator();
