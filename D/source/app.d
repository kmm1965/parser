import std.stdio: writeln;

import calculator: calculate;

void main()
{
    calculate("72 - 7 - (1 - 2) * 3").writeln;
    calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").writeln;
    calculate("3^(1+1)^3").writeln;
    calculate("sin(1+1)").writeln;
    calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").writeln;
    calculate("sqr(2 + 3)").writeln;
}
