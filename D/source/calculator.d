import parser;
import some_parsers;
import maybe;
import std.typecons: Tuple, tuple;

@safe @nogc double sqr(double x) pure nothrow {
    return x * x;
}

Parser!T def_object(T)(string n, T x) pure {
    return name(n) >> Parser_pure(x);
}

auto func() pure
{
    import std.math: sin, cos, asin, acos, sinh, cosh, asinh, acosh, tan, atan, log, exp, sqrt;

    return def_object("sin",   (double x) => sin(x)) |
           def_object("cos",   (double x) => cos(x)) |
           def_object("asin",  (double x) => acos(x)) |
           def_object("acos",  (double x) => acos(x)) |
           def_object("sinh",  (double x) => sinh(x)) |
           def_object("cosh",  (double x) => cosh(x)) |
           def_object("asinh", (double x) => acosh(x)) |
           def_object("acosh", (double x) => acosh(x)) |
           def_object("tan",   (double x) => tan(x)) |
           def_object("atan",  (double x) => atan(x)) |
           def_object("log",   (double x) => log(x)) |
           def_object("exp",   (double x) => exp(x)) |
           def_object("sqrt",  (double x) => sqrt(x)) |
           def_object("sqr",   (double x) => sqr(x));
}

auto _const() pure
{
    import std.math.constants: E, LOG2E, LOG10E, LN2, LN10, PI, PI_2, PI_4, M_1_PI, M_2_PI, M_2_SQRTPI, SQRT2, SQRT1_2;

    return def_object("E",        cast(double)E) |
           def_object("LOG2E",    cast(double)LOG2E) |
           def_object("LOG10E",   cast(double)LOG10E) |
           def_object("LN2",      cast(double)LN2) |
           def_object("LN10",     cast(double)LN10) |
           def_object("PI",       cast(double)PI) |
           def_object("PI_2",     cast(double)PI_2) |
           def_object("PI_4",     cast(double)PI_4) |
           def_object("1_PI",     cast(double)M_1_PI) |
           def_object("2_PI",     cast(double)M_2_PI) |
           def_object("2_SQRTPI", cast(double)M_2_SQRTPI) |
           def_object("SQRT2",    cast(double)SQRT2) |
           def_object("SQRT1_2",  cast(double)SQRT1_2);
}

auto op2(alias func)(char c) pure {
    return symbol(c) >> Parser_pure(func);
}

auto add() pure {
    return op2!((double x, double y) => x + y)('+');
}

auto sub() pure {
    return op2!((double x, double y) => x - y)('-');
}

auto mul() pure {
    return op2!((double x, double y) => x * y)('*');
}

auto div() pure {
    return op2!((double x, double y) => x / y)('/');
}

auto pow() pure {
    import std.math: exp, log;

    return op2!((double x, double y) => exp(y * log(x)))('^');
}

Parser!double expr() pure {
    return term().chainl1(add | sub);
}

Parser!double term() pure {
    return factor().chainl1(mul | div);
}

Parser!double factor() pure {
    return factor0().chainr1(pow);
}

Parser!double factor0() pure {
    return expr_in_brackets |
           func * (() => expr_in_brackets) |
           _const |
           double_;
}

Parser!double expr_in_brackets() pure {
    return between!(() => expr)(symbol('('), symbol(')'));
}

Maybe!(Tuple!(double, string)) calculate(string s) pure {
    return expr.parse(s);
}

@("calculate unit test")
unittest {
    import std.math: sin;
    import maybe: transform;
    import std.math.rounding: round;

    assert(calculate("72 - 7 - (1 - 2) * 3") == Just(tuple(68., "")));
    assert(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") == Just(tuple(-8.889, "")));
    assert(calculate("3^(1+1)^3").transform!(pair => tuple(round(pair[0]), pair[1])) == Just(tuple(6561., "")));
    assert(calculate("sin(1+1)") == Just(tuple(sin(2.), "")));
    assert(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )") == Just(tuple(-0.3634085731426532, "")));
    assert(calculate("sqr(2 + 3)") == Just(tuple(25., "")));
}
