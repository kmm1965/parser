import parser;
import some_parsers;
import maybe;
import std.typecons: Tuple, tuple;

@safe @nogc double sqr(double x) pure nothrow {
    return x * x;
}

Parser!T guard(T)(bool b, T x) pure {
    return b ? Parser_pure(x) : Parser!T.empty;
}

auto funcs() pure
{
    import std.math: sin, cos, asin, acos, sinh, cosh, asinh, acosh, tan, atan, log, log10, exp, sqrt;

    return identifier.and_then!(n =>
        guard(n == "sin",   (double x) => sin(x)) |
        guard(n == "cos",   (double x) => cos(x)) |
        guard(n == "asin",  (double x) => asin(x)) |
        guard(n == "acos",  (double x) => acos(x)) |
        guard(n == "sinh",  (double x) => sinh(x)) |
        guard(n == "cosh",  (double x) => cosh(x)) |
        guard(n == "asinh", (double x) => asinh(x)) |
        guard(n == "acosh", (double x) => acosh(x)) |
        guard(n == "tan",   (double x) => tan(x)) |
        guard(n == "atan",  (double x) => atan(x)) |
        guard(n == "log",   (double x) => log(x)) |
        guard(n == "log10", (double x) => log10(x)) |
        guard(n == "exp",   (double x) => exp(x)) |
        guard(n == "sqrt",  (double x) => sqrt(x)) |
        guard(n == "sqr",   (double x) => sqr(x)));
}

@("funcs unit test")
unittest {
    import std.math: sin, cos, asin, acos, sinh, cosh, asinh, acosh, tan, atan, log, log10, exp, sqrt;

    assert(calculate("sin(2.0)") == Just(tuple(sin(2.0), "")));
    assert(calculate("cos(2.0)") == Just(tuple(cos(2.0), "")));
    assert(calculate("asin(0.5)") == Just(tuple(asin(0.5), "")));
    assert(calculate("acos(0.5)") == Just(tuple(acos(0.5), "")));
    assert(calculate("sinh(2.0)") == Just(tuple(sinh(2.0), "")));
    assert(calculate("cosh(2.0)") == Just(tuple(cosh(2.0), "")));
    assert(calculate("asinh(2.0)") == Just(tuple(asinh(2.0), "")));
    assert(calculate("acosh(2.0)") == Just(tuple(acosh(2.0), "")));
    assert(calculate("tan(2.0)") == Just(tuple(tan(2.0), "")));
    assert(calculate("log(2.0)") == Just(tuple(log(2.0), "")));
    assert(calculate("log10(2.0)") == Just(tuple(log10(2.0), "")));
    assert(calculate("exp(2.0)") == Just(tuple(exp(2.0), "")));
    assert(calculate("sqrt(2.0)") == Just(tuple(sqrt(2.0), "")));
    assert(calculate("sqr(2.0)") == Just(tuple(4.0, "")));
}

auto consts() pure
{
    import std.math.constants: E, LOG2E, LOG10E, LN2, LN10, PI, PI_2, PI_4, M_1_PI, M_2_PI, M_2_SQRTPI, SQRT2, SQRT1_2;

    return identifier.and_then!(n =>
        guard(n == "E",        cast(double)E) |
        guard(n == "LOG2E",    cast(double)LOG2E) |
        guard(n == "LOG10E",   cast(double)LOG10E) |
        guard(n == "LN2",      cast(double)LN2) |
        guard(n == "LN10",     cast(double)LN10) |
        guard(n == "PI",       cast(double)PI) |
        guard(n == "PI_2",     cast(double)PI_2) |
        guard(n == "PI_4",     cast(double)PI_4) |
        guard(n == "1_PI",     cast(double)M_1_PI) |
        guard(n == "2_PI",     cast(double)M_2_PI) |
        guard(n == "2_SQRTPI", cast(double)M_2_SQRTPI) |
        guard(n == "SQRT2",    cast(double)SQRT2) |
        guard(n == "SQRT1_2",  cast(double)SQRT1_2));
}

@("consts unit test")
unittest {
    import std.math.constants: E, LOG2E, LOG10E, LN2, LN10, PI, PI_2, PI_4, M_1_PI, M_2_PI, M_2_SQRTPI, SQRT2, SQRT1_2;
    import std.math: log, sqrt;

    assert(calculate("E") == Just(tuple(cast(double)E, "")));
    assert(calculate("LOG2E") == Just(tuple(cast(double)LOG2E, "")));
    assert(calculate("LOG2E") == Just(tuple(1 / cast(double)LN2, "")));
    assert(calculate("LOG10E") == Just(tuple(cast(double)LOG10E, "")));
    assert(calculate("LOG10E") == Just(tuple(1 / cast(double)LN10, "")));
    assert(calculate("LN2") == Just(tuple(cast(double)LN2, "")));
    assert(calculate("LN2") == Just(tuple(log(2.0), "")));
    assert(calculate("LN10") == Just(tuple(cast(double)LN10, "")));
    assert(calculate("LN10") == Just(tuple(log(10.0), "")));
    assert(calculate("PI") == Just(tuple(cast(double)PI, "")));
    assert(calculate("PI_2") == Just(tuple(cast(double)PI_2, "")));
    assert(calculate("PI_2") == Just(tuple(cast(double)PI / 2, "")));
    assert(calculate("PI_4") == Just(tuple(cast(double)PI_4, "")));
    assert(calculate("PI_4") == Just(tuple(cast(double)PI / 4, "")));
    assert(calculate("1_PI") == Just(tuple(cast(double)M_1_PI, "")));
    assert(calculate("1_PI") == Just(tuple(1 / cast(double)PI, "")));
    assert(calculate("2_PI") == Just(tuple(cast(double)M_2_PI, "")));
    assert(calculate("2_PI") == Just(tuple(2 / cast(double)PI, "")));
    assert(calculate("2_SQRTPI") == Just(tuple(cast(double)M_2_SQRTPI, "")));
    assert(calculate("2_SQRTPI") == Just(tuple(2 / sqrt(cast(double)PI), "")));
    assert(calculate("SQRT2") == Just(tuple(cast(double)SQRT2, "")));
    assert(calculate("SQRT2") == Just(tuple(sqrt(2.0), "")));
    assert(calculate("SQRT1_2") == Just(tuple(cast(double)SQRT1_2, "")));
    assert(calculate("SQRT1_2") == Just(tuple(sqrt(0.5), "")));
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
    return usign.and_then!(sgn => chainl1(term(), add | sub, sgn == "-"));
}

Parser!double term() pure {
    return chainl1(factor(), mul | div, false);
}

Parser!double factor() pure {
    return factor0().chainr1(pow);
}

Parser!double factor0() pure {
    return expr_in_brackets |
           funcs * (() => expr_in_brackets) |
           consts |
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
