import parser;
import maybe;

import std.typecons: Tuple, tuple;

Parser!char alnum() pure {
    import std.ascii: isAlphaNum;
    return satisfy!(c => isAlphaNum(c) || c == '_');
}

@("alnum unit test")
unittest {
    assert(alnum.parse("123abc  ") == Just(tuple('1', "23abc  ")));
    assert(alnum.parse("_123  abc") == Just(tuple('_', "123  abc")));
    assert(alnum.parse("!@#$") == Nothing!(Tuple!(char, string)));
}

Parser!char _char(char c) pure {
    return satisfy!(x => x == c);
}

@("_char unit test")
unittest {
    assert(_char('a').parse("abc") == Just(tuple('a', "bc")));
    assert(_char('z').parse("abc") == Nothing!(Tuple!(char, string)));
}

Parser!char symbol(char c) pure {
    return _char(c).token;
}

@("symbol unit test")
unittest {
    assert(symbol('+').parse(" + abc") == Just(tuple('+', "abc")));
    assert(symbol('+').parse("abc") == Nothing!(Tuple!(char, string)));
}

Parser!string name(string n) pure {
    return alnum.some.and_then!(s => s == n ? Parser_pure(n) : Parser!string.empty).token;
}

@("name unit test")
unittest {
    assert(name("sin").parse(" sin ") == Just(tuple("sin", "")));
    assert(name("sin").parse("  sin  (1.)") == Just(tuple("sin", "(1.)")));
    assert(name("sin").parse("sinabc") == Nothing!(Tuple!(string, string)));
}

Parser!string optional_s(const Parser!string p) pure {
    return p | empty_string;
}

Parser!string optional_c(const Parser!char p) pure {
    import std.conv: to;

    return optional_s(((char c) => c.to!string) / p);
}

@("optional_s unit test")
unittest {
    assert(name("sin").optional_s.parse(" sin abc") == Just(tuple("sin", "abc")));
    assert(name("sin").optional_s.parse("abc") == Just(tuple("", "abc")));
    assert(_char('1').optional_c.parse("1234") == Just(tuple("1", "234")));
    assert(_char('1').optional_c.parse("abc") == Just(tuple("", "abc")));
}

Parser!string sign() pure {
    return optional_c(_char('+') | _char('-'));
}

@("sign unit test")
unittest {
    assert(sign.parse("abc") == Just(tuple("", "abc")));
    assert(sign.parse("+abc") == Just(tuple("+", "abc")));
    assert(sign.parse("-abc") == Just(tuple("-", "abc")));
}

Parser!string digits() pure {
    import std.ascii: isDigit;
    return satisfy!(c => isDigit(c)).many;
}

@("digits unit test")
unittest {
    assert(digits.parse("123abc") == Just(tuple("123", "abc")));
    assert(digits.parse("123  abc") == Just(tuple("123", "  abc")));
    assert(digits.parse("abc") == Just(tuple("", "abc")));
}

Parser!double _double() pure {
    import std.ascii: isDigit;
    import std.conv: to;

    return sign.and_then!(
        sign_part => digits.and_then!(
        int_part  => optional_s(_char('.') >> digits).and_then!(
        frac_part => optional_s(((_char('e') | _char('E')) >> sign).and_then!(
               exp_sign   => satisfy!(c => isDigit(c)).some.and_then!(
               exp_digits => Parser_pure(exp_sign ~ exp_digits)
            ))).and_then!(
        exp_part  => int_part.length > 0 || frac_part.length > 0 ?
            Parser_pure((sign_part ~ int_part ~
                (frac_part.length > 0 ? '.' ~ frac_part : "") ~
                (exp_part.length > 0 ? 'e' ~ exp_part : "")).to!double) :
            Parser!double.empty
    )))).token;
}

@("_double unit test")
unittest {
    assert(_double.parse(" 1 abc") == Just(tuple(1., "abc")));
    assert(_double.parse(" 1. abc") == Just(tuple(1., "abc")));
    assert(_double.parse(" 1.23 abc") == Just(tuple(1.23, "abc")));
    assert(_double.parse(" .23 abc") == Just(tuple(0.23, "abc")));
    assert(_double.parse(" +1.23 abc") == Just(tuple(+1.23, "abc")));
    assert(_double.parse(" -1.23 abc") == Just(tuple(-1.23, "abc")));
    assert(_double.parse("1.23e10abc") == Just(tuple(1.23e10, "abc")));
    assert(_double.parse("1.23e-10abc") == Just(tuple(1.23e-10, "abc")));
    assert(_double.parse("-1.23e-10abc") == Just(tuple(-1.23e-10, "abc")));
    assert(_double.parse("abc") == Nothing!(Tuple!(double, string)));
}

@("between unit test")
unittest {
    auto expr = between!_double(symbol('('), symbol(')'));
    assert(expr.parse(" ( 123 ) abc") == Just(tuple(123., "abc")));
    assert(expr.parse(" ( 123 abc") == Nothing!(Tuple!(double, string)));
    assert(expr.parse(" 123 ) abc") == Nothing!(Tuple!(double, string)));
}

@("chainlr1 unit test")
unittest {
    import std.math: exp, log;
    import std.math.rounding: round;
    import maybe: transform;

    auto add = symbol('+') >> Parser_pure((double x, double y) => x + y);
    auto sub = symbol('-') >> Parser_pure((double x, double y) => x - y);
    auto pow = symbol('^') >> Parser_pure((double x, double y) => exp(y * log(x)));
    auto expr = _double.chainl1(add | sub);

    assert(expr.parse("7abc") == Just(tuple(7., "abc")));
    assert(expr.parse(" 7 - 1 - 2 abc") == Just(tuple(4., "abc")));
    assert(expr.parse(" 7 - 1 + 2 - 3 abc") == Just(tuple(5., "abc")));
    assert(expr.parse("abc") == Nothing!(Tuple!(double, string)));

    assert(_double.chainr1!double(pow).parse("3 ^ 2 ^ 3 abc")
        .transform!(pair => tuple(round(pair[0]), pair[1])) == Just(tuple(6561., "abc")));
}
