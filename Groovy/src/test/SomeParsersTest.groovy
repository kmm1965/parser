package test

import main.SomeParsers
import main.Parser

static test_anyChar() {
    assert(SomeParsers.anyChar.parse("abc") == Optional.of(new Tuple2('a' as char, "bc")))
    assert(SomeParsers.anyChar.parse("") == Optional.empty())
}

static test_satisfy() {
    assert(SomeParsers.satisfy { char c -> c == 'a' as char }.parse("abc") == Optional.of(new Tuple2('a' as char, "bc")))
    assert(SomeParsers.satisfy { char c -> c == 'z' as char }.parse("abc") == Optional.empty())
    assert(SomeParsers.satisfy { char c -> c == 'a' as char }.parse("") == Optional.empty())
}

static test_char() {
    assert(SomeParsers.char_('a' as char).parse("abc") == Optional.of(new Tuple2('a' as char, "bc")))
    assert(SomeParsers.char_('z' as char).parse("abc") == Optional.empty())
    assert(SomeParsers.char_('a' as char).parse("") == Optional.empty())
}

static test_empty_string() {
    assert(SomeParsers.empty_string.parse("abc") == Optional.of(new Tuple2("", "abc")))
}

static test_optional() {
    assert(SomeParsers.optional_c(SomeParsers.char_('1' as char)).parse("1234") == Optional.of(new Tuple2("1", "234")))
    assert(SomeParsers.optional_c(SomeParsers.char_('1' as char)).parse("abc") == Optional.of(new Tuple2("", "abc")))
}

static test_spaces() {
    assert(SomeParsers.spaces.parse("abc") == Optional.of(new Tuple2("", "abc")))
    assert(SomeParsers.spaces.parse("   abc") == Optional.of(new Tuple2("   ", "abc")))
}

static test_symbol() {
    assert(SomeParsers.symbol('+' as char).parse(" + abc") == Optional.of(new Tuple2('+' as char, "abc")))
    assert(SomeParsers.symbol('+' as char).parse("abc") == Optional.empty())
}


static test_alnum() {
    assert(SomeParsers.alnum.parse("123abc") == Optional.of(new Tuple2('1' as char, "23abc")))
    assert(SomeParsers.alnum.parse("_123abc") == Optional.of(new Tuple2('_' as char, "123abc")))
    assert(SomeParsers.alnum.parse("!@#") == Optional.empty())
}

static test_name() {
    Parser psin = SomeParsers.name("sin")

    assert(psin.parse(" sin ") == Optional.of(new Tuple2("sin", "")))
    assert(psin.parse(" sin (1.)") == Optional.of(new Tuple2("sin", "(1.)")))
    assert(psin.parse("sinus") == Optional.empty())
}

static test_sign() {
    assert(SomeParsers.sign.parse("abc") == Optional.of(new Tuple2("", "abc")))
    assert(SomeParsers.sign.parse("+abc") == Optional.of(new Tuple2("+", "abc")))
    assert(SomeParsers.sign.parse("-abc") == Optional.of(new Tuple2("-", "abc")))

    assert(SomeParsers.usign.parse("abc") == Optional.of(new Tuple2("", "abc")))
    assert(SomeParsers.usign.parse(" + abc") == Optional.of(new Tuple2("+", "abc")))
    assert(SomeParsers.usign.parse(" - abc") == Optional.of(new Tuple2("-", "abc")))
}

static test_digits() {
    assert(SomeParsers.digits.parse("123abc") == Optional.of(new Tuple2("123", "abc")))
    assert(SomeParsers.digits.parse("123  abc") == Optional.of(new Tuple2("123", "  abc")))
    assert(SomeParsers.digits.parse("abc") == Optional.of(new Tuple2("", "abc")))
}

static test_double() {
    assert(SomeParsers.double_.parse("1 abc") == Optional.of(new Tuple2(1.0, "abc")))
    assert(SomeParsers.double_.parse("1. abc") == Optional.of(new Tuple2(1.0, "abc")))
    assert(SomeParsers.double_.parse("1.23 abc") == Optional.of(new Tuple2(1.23, "abc")))
    assert(SomeParsers.double_.parse("-1.23 abc") == Optional.empty())
    assert(SomeParsers.double_.parse(".23 abc") == Optional.of(new Tuple2(0.23, "abc")))
    assert(SomeParsers.double_.parse(" + 1.23 abc") == Optional.empty())
    assert(SomeParsers.double_.parse("1.23e10abc") == Optional.of(new Tuple2(1.23e10, "abc")))
    assert(SomeParsers.double_.parse("1.23e-10abc") == Optional.of(new Tuple2(1.23e-10, "abc")))
    assert(SomeParsers.double_.parse("abc") == Optional.empty())
}

static test_between() {
    Parser expr = SomeParsers.between(SomeParsers.symbol('(' as char), SomeParsers.symbol(')' as char)) { SomeParsers.double_ }

    assert(expr.parse(" ( 123 ) abc") == Optional.of(new Tuple2(123.0, "abc")))
    assert(expr.parse(" ( 123 abc") == Optional.empty())
    assert(expr.parse(" 123 ) abc") == Optional.empty())
}

static test_chainlr1() {
    Parser add = SomeParsers.symbol('+' as char).skip { Parser.pure { Double x, Double y -> x + y } }
    Parser sub = SomeParsers.symbol('-' as char).skip { Parser.pure { Double x, Double y -> x - y } }
    Parser pow = SomeParsers.symbol('^' as char).skip { Parser.pure { Double x, Double y -> Math.exp(y * Math.log(x)) } }

    Parser pexpr = SomeParsers.chainl1(SomeParsers.double_, add.orElse { sub }, false)

    assert(pexpr.parse("7abc") == Optional.of(new Tuple2(7.0, "abc")))
    assert(pexpr.parse(" 7 - 1 - 2 abc") == Optional.of(new Tuple2(4.0, "abc")))
    assert(pexpr.parse(" 7 - 1 + 2 - 3 abc") == Optional.of(new Tuple2(5.0, "abc")))
    assert(pexpr.parse("abc") == Optional.empty())

    assert(SomeParsers.chainr1(SomeParsers.double_, pow).parse("3 ^ 2 ^ 3 abc")
        .map { Tuple2<Double, String> pair -> new Tuple2(pair.getV1().round(), pair.getV2()) } ==
        Optional.of(new Tuple2(6561.0, "abc")))
}

test_anyChar()
test_satisfy()
test_char()
test_empty_string()
test_optional()
test_spaces()
test_symbol()
test_alnum()
test_name()
test_sign()
test_digits()
test_double()
test_between()
test_chainlr1()