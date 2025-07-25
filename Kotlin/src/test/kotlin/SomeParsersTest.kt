import SomeParsers.Companion.alnum
import SomeParsers.Companion.anyChar
import SomeParsers.Companion.between
import SomeParsers.Companion.chainl1
import SomeParsers.Companion.chainr1
import SomeParsers.Companion.char
import SomeParsers.Companion.digits
import SomeParsers.Companion.double
import SomeParsers.Companion.empty_string
import SomeParsers.Companion.name
import SomeParsers.Companion.optional_c
import SomeParsers.Companion.satisfy
import SomeParsers.Companion.sign
import SomeParsers.Companion.spaces
import SomeParsers.Companion.symbol
import SomeParsers.Companion.usign
import arrow.core.Some
import arrow.core.none
import kotlin.math.exp
import kotlin.math.ln
import kotlin.math.round
import kotlin.test.Test
import kotlin.test.assertEquals

class SomeParsersTest {
    @Test
    fun `test anyChar` () {
        assertEquals(Some(Pair('a', "bc")), anyChar.parse("abc"))
        assertEquals(none(), anyChar.parse(""))
    }

    @Test
    fun `test satisfy` () {
        assertEquals(Some(Pair('a', "bc")), satisfy { c -> c == 'a' }.parse("abc"))
        assertEquals(none(), satisfy { c -> c == 'z' }.parse("abc"))
        assertEquals(none(), satisfy { c -> c == 'a' }.parse(""))
    }

    @Test
    fun `test char` () {
        assertEquals(Some(Pair('a', "bc")), char('a').parse("abc"))
        assertEquals(none(), char('z').parse("abc"))
        assertEquals(none(), char('a').parse(""))
    }

    @Test
    fun `test empty_string` () {
        assertEquals(Some(Pair("", "abc")), empty_string.parse("abc"))
    }

    @Test
    fun `test optional` () {
        assertEquals(Some(Pair("1", "234")), optional_c(char('1')).parse("1234"))
        assertEquals(Some(Pair("", "abc")), optional_c(char('1')).parse("abc"))
    }

    @Test
    fun `test spaces` () {
        assertEquals(Some(Pair("", "abc")), spaces.parse("abc"))
        assertEquals(Some(Pair("   ", "abc")), spaces.parse("   abc"))
    }

    @Test
    fun `test symbol` () {
        assertEquals(Some(Pair('+', "abc")), symbol('+').parse(" + abc"))
        assertEquals(none(), symbol('+').parse("abc"))
    }

    @Test
    fun `test alnum` () {
        assertEquals(Some(Pair('1', "23abc")), alnum.parse("123abc"))
        assertEquals(Some(Pair('_', "123abc")), alnum.parse("_123abc"))
        assertEquals(none(), alnum.parse("!@#"))
    }

    @Test
    fun `test name` () {
        val psin = name("sin")

        assertEquals(Some(Pair("sin", "")), psin.parse(" sin "))
        assertEquals(Some(Pair("sin", "(1.)")), psin.parse(" sin (1.)"))
        assertEquals(none(), psin.parse("sinus"))
    }

    @Test
    fun `test sign` () {
        assertEquals(Some(Pair("", "abc")), sign.parse("abc"))
        assertEquals(Some(Pair("+", "abc")), sign.parse("+abc"))
        assertEquals(Some(Pair("-", "abc")), sign.parse("-abc"))

        assertEquals(Some(Pair("", "abc")), usign.parse("abc"))
        assertEquals(Some(Pair("+", "abc")), usign.parse(" + abc"))
        assertEquals(Some(Pair("-", "abc")), usign.parse(" - abc"))
    }

    @Test
    fun `test digits` () {
        assertEquals(Some(Pair("123", "abc")), digits.parse("123abc"))
        assertEquals(Some(Pair("123", "  abc")), digits.parse("123  abc"))
        assertEquals(Some(Pair("", "abc")), digits.parse("abc"))
    }

    @Test
    fun `test double` () {
        assertEquals(Some(Pair(1.0, "abc")), double.parse("1 abc"))
        assertEquals(Some(Pair(1.0, "abc")), double.parse("1. abc"))
        assertEquals(Some(Pair(1.23, "abc")), double.parse("1.23 abc"))
        assertEquals(Some(Pair(-1.23, "abc")), double.parse("-1.23 abc"))
        assertEquals(Some(Pair(0.23, "abc")), double.parse(".23 abc"))
        assertEquals(none(), double.parse(" + 1.23 abc"))
        assertEquals(Some(Pair(1.23e10, "abc")), double.parse("1.23e10abc"))
        assertEquals(Some(Pair(1.23e-10, "abc")), double.parse("1.23e-10abc"))
        assertEquals(none(), double.parse("abc"))
    }

    @Test
    fun `test between` () {
        val expr = between(symbol('('), symbol(')')) { double }

        assertEquals(Some(Pair(123.0, "abc")), expr.parse(" ( 123 ) abc"))
        assertEquals(none(), expr.parse(" ( 123 abc"))
        assertEquals(none(), expr.parse(" 123 ) abc"))
    }

    @Test
    fun `test chainlr1` () {
        val add = symbol('+').skip { Parser.pure { x: Double, y: Double -> x + y } }
        val sub = symbol('-').skip { Parser.pure { x: Double, y: Double -> x - y } }
        val pow = symbol('^').skip { Parser.pure { x: Double, y: Double -> exp(y * ln(x)) } }

        val pexpr = chainl1(double, add.orElse { sub }, false)

        assertEquals(Some(Pair(7.0, "abc")), pexpr.parse("7abc"))
        assertEquals(Some(Pair(4.0, "abc")), pexpr.parse(" 7 - 1 - 2 abc"))
        assertEquals(Some(Pair(5.0, "abc")), pexpr.parse(" 7 - 1 + 2 - 3 abc"))
        assertEquals(none(), pexpr.parse("abc"))

        assertEquals(Some(Pair(6561.0, "abc")),
            chainr1(double, pow).parse("3 ^ 2 ^ 3 abc").map { (a, out) -> Pair(round(a), out) })
    }
}
