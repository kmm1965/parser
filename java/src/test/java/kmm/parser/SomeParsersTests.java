package kmm.parser;

import kmm.utils.Pair;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.function.BinaryOperator;

import static kmm.parser.Parser.*;
import static kmm.parser.SomeParsers.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SomeParsersTests {
    @Test
    public void test_anyChar() {
        assertEquals(Optional.of(Pair.of('a', "bc")), anyChar.parse("abc"));
        assertEquals(Optional.empty(), anyChar.parse(""));
    }

    @Test
    public void test_satisfy() {
        assertEquals(Optional.of(Pair.of('a', "bc")), satisfy(c -> c == 'a').parse("abc"));
        assertEquals(Optional.empty(), satisfy(c -> c == 'z').parse("abc"));
        assertEquals(Optional.empty(), satisfy(c -> c == 'a').parse(""));
    }

    @Test
    public void test_char() {
        assertEquals(Optional.of(Pair.of('a', "bc")), char_('a').parse("abc"));
        assertEquals(Optional.empty(), char_('z').parse("abc"));
        assertEquals(Optional.empty(), char_('a').parse(""));
    }

    @Test
    public void test_emptyString() {
        assertEquals(Optional.of(Pair.of("", "abc")), Parser.emptyString.parse("abc"));
    }

    @Test
    public void test_optional() {
        assertEquals(Optional.of(Pair.of("1", "234")), optional_c(char_('1')).parse("1234"));
        assertEquals(Optional.of(Pair.of("", "abc")), optional_c(char_('1')).parse("abc"));
    }

    @Test
    public void test_spaces() {
        assertEquals(Optional.of(Pair.of("", "abc")), Parser.spaces.parse("abc"));
        assertEquals(Optional.of(Pair.of("   ", "abc")), Parser.spaces.parse("   abc"));
    }

    @Test
    public void test_symbol() {
        Parser<Character> splus = symbol('+');
        assertEquals(Optional.of(Pair.of('+', "abc")), splus.parse(" + abc"));
        assertEquals(Optional.empty(), splus.parse("abc"));
    }

    @Test
    public void test_alnum() {
        assertEquals(Optional.of(Pair.of('1', "23abc")), alnum.parse("123abc"));
        assertEquals(Optional.of(Pair.of('a', "bc")), alnum.parse("abc"));
        assertEquals(Optional.of(Pair.of('_', "123abc")), alnum.parse("_123abc"));
        assertEquals(Optional.empty(), alnum.parse("!@#"));
    }

    @Test
    public void test_sign() {
        assertEquals(Optional.of(Pair.of("", "abc")), sign.parse("abc"));
        assertEquals(Optional.of(Pair.of("+", "abc")), sign.parse("+abc"));
        assertEquals(Optional.of(Pair.of("-", "abc")), sign.parse("-abc"));

        assertEquals(Optional.of(Pair.of("", "abc")), usign.parse("abc"));
        assertEquals(Optional.of(Pair.of("+", "abc")), usign.parse(" + abc"));
        assertEquals(Optional.of(Pair.of("-", "abc")), usign.parse(" - abc"));
    }

    @Test
    public void test_digits() {
        assertEquals(Optional.of(Pair.of("123", "abc")), digits.parse("123abc"));
        assertEquals(Optional.of(Pair.of("123", "   abc")), digits.parse("123   abc"));
        assertEquals(Optional.of(Pair.of("", "abc")), digits.parse("abc"));
    }

    @Test
    public void test_double() {
        assertEquals(Optional.of(Pair.of(1.0, "abc")), double_.parse("1 abc"));
        assertEquals(Optional.of(Pair.of(1.0, "abc")), double_.parse("1. abc"));
        assertEquals(Optional.of(Pair.of(1.23, "abc")), double_.parse("1.23 abc"));
        assertEquals(Optional.empty(), double_.parse("-1.23 abc"));
        assertEquals(Optional.of(Pair.of(0.23, "abc")), double_.parse(".23 abc"));
        assertEquals(Optional.empty(), double_.parse(" + 1.23 abc"));
        assertEquals(Optional.of(Pair.of(1.23e10, "abc")), double_.parse("1.23e10abc"));
        assertEquals(Optional.of(Pair.of(1.23e-10, "abc")), double_.parse("1.23e-10abc"));
        assertEquals(Optional.empty(), double_.parse("abc"));
    }

    @Test
    public void test_between() {
        Parser<Double> expr = between(symbol('('), symbol(')'), () -> double_);
        assertEquals(Optional.of(Pair.of(123.0, "abc")), expr.parse(" ( 123 ) abc"));
        assertEquals(Optional.empty(), expr.parse(" ( 123 abc"));
        assertEquals(Optional.empty(), expr.parse(" 123 ) abc"));
    }

    @Test
    public void test_chainlr1() {
        Parser<BinaryOperator<Double>>
            add = symbol('+').skip(() -> Parser.pure(Double::sum)),
            sub = symbol('-').skip(() -> Parser.pure((x, y) -> x - y)),
            pow = symbol('^').skip(() -> Parser.pure(Math::pow));

        Parser<Double> pexpr = chainl1(double_, add.orElse(sub), false);

        assertEquals(Optional.of(Pair.of(7.0, "")), pexpr.parse("7"));
        assertEquals(Optional.of(Pair.of(7.0, "abc")), pexpr.parse("7abc"));
        assertEquals(Optional.of(Pair.of(6.0, "")), pexpr.parse("7-1"));
        assertEquals(Optional.of(Pair.of(4.0, "abc")), pexpr.parse(" 7 - 1 - 2 abc"));
        assertEquals(Optional.of(Pair.of(5.0, "abc")), pexpr.parse(" 7 - 1 + 2 - 3 abc"));
        assertEquals(Optional.empty(), pexpr.parse("abc"));
        assertEquals(Optional.of(Pair.of(6561.0, "abc")), double_.chainr1(pow).parse("3 ^ 2 ^ 3 abc"));
    }
}
