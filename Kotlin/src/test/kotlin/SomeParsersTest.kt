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
import SomeParsers.Companion.optionalC
import SomeParsers.Companion.satisfy
import SomeParsers.Companion.sign
import SomeParsers.Companion.spaces
import SomeParsers.Companion.symbol
import SomeParsers.Companion.usign
import java.util.Optional
import kotlin.math.exp
import kotlin.math.ln
import kotlin.math.round
import kotlin.test.Test
import kotlin.test.assertEquals

class SomeParsersTest {
    @Test
    fun `test anyChar` () {
        assertEquals(Optional.of(Pair('a', "bc")), anyChar.parse("abc"))
        assertEquals(Optional.empty(), anyChar.parse(""))
    }

    @Test
    fun `test satisfy` () {
        assertEquals(Optional.of(Pair('a', "bc")), satisfy { c -> c == 'a' }.parse("abc"))
        assertEquals(Optional.empty(), satisfy { c -> c == 'z' }.parse("abc"))
        assertEquals(Optional.empty(), satisfy { c -> c == 'a' }.parse(""))
    }

    @Test
    fun `test char` () {
        assertEquals(Optional.of(Pair('a', "bc")), char('a').parse("abc"))
        assertEquals(Optional.empty(), char('z').parse("abc"))
        assertEquals(Optional.empty(), char('a').parse(""))
    }

    @Test
    fun `test empty_string` () {
        assertEquals(Optional.of(Pair("", "abc")), empty_string.parse("abc"))
    }

    @Test
    fun `test optional` () {
        assertEquals(Optional.of(Pair("1", "234")), optionalC(char('1')).parse("1234"))
        assertEquals(Optional.of(Pair("", "abc")), optionalC(char('1')).parse("abc"))
    }

    @Test
    fun `test spaces` () {
        assertEquals(Optional.of(Pair("", "abc")), spaces.parse("abc"))
        assertEquals(Optional.of(Pair("   ", "abc")), spaces.parse("   abc"))
    }

    @Test
    fun `test symbol` () {
        assertEquals(Optional.of(Pair('+', "abc")), symbol('+').parse(" + abc"))
        assertEquals(Optional.empty(), symbol('+').parse("abc"))
    }

    @Test
    fun `test alnum` () {
        assertEquals(Optional.of(Pair('1', "23abc")), alnum.parse("123abc"))
        assertEquals(Optional.of(Pair('_', "123abc")), alnum.parse("_123abc"))
        assertEquals(Optional.empty(), alnum.parse("!@#"))
    }

    @Test
    fun `test name` () {
        val psin = name("sin")

        assertEquals(Optional.of(Pair("sin", "")), psin.parse(" sin "))
        assertEquals(Optional.of(Pair("sin", "(1.)")), psin.parse(" sin (1.)"))
        assertEquals(Optional.empty(), psin.parse("sinus"))
    }

    @Test
    fun `test sign` () {
        assertEquals(Optional.of(Pair("", "abc")), sign.parse("abc"))
        assertEquals(Optional.of(Pair("+", "abc")), sign.parse("+abc"))
        assertEquals(Optional.of(Pair("-", "abc")), sign.parse("-abc"))

        assertEquals(Optional.of(Pair("", "abc")), usign.parse("abc"))
        assertEquals(Optional.of(Pair("+", "abc")), usign.parse(" + abc"))
        assertEquals(Optional.of(Pair("-", "abc")), usign.parse(" - abc"))
    }

    @Test
    fun `test digits` () {
        assertEquals(Optional.of(Pair("123", "abc")), digits.parse("123abc"))
        assertEquals(Optional.of(Pair("123", "  abc")), digits.parse("123  abc"))
        assertEquals(Optional.of(Pair("", "abc")), digits.parse("abc"))
    }

    @Test
    fun `test double` () {
        assertEquals(Optional.of(Pair(1.0, "abc")), double.parse("1 abc"))
        assertEquals(Optional.of(Pair(1.0, "abc")), double.parse("1. abc"))
        assertEquals(Optional.of(Pair(1.23, "abc")), double.parse("1.23 abc"))
        assertEquals(Optional.of(Pair(0.23, "abc")), double.parse(".23 abc"))
        assertEquals(Optional.empty(), double.parse(" + 1.23 abc"))
        assertEquals(Optional.of(Pair(1.23e10, "abc")), double.parse("1.23e10abc"))
        assertEquals(Optional.of(Pair(1.23e-10, "abc")), double.parse("1.23e-10abc"))
        assertEquals(Optional.empty(), double.parse("abc"))
    }

    @Test
    fun `test between` () {
        val expr = between(symbol('('), symbol(')')) { double }

        assertEquals(Optional.of(Pair(123.0, "abc")), expr.parse(" ( 123 ) abc"))
        assertEquals(Optional.empty(), expr.parse(" ( 123 abc"))
        assertEquals(Optional.empty(), expr.parse(" 123 ) abc"))
    }

    @Test
    fun `test chainlr1` () {
        val add = symbol('+').skip { Parser.pure { x: Double, y: Double -> x + y } }
        val sub = symbol('-').skip { Parser.pure { x: Double, y: Double -> x - y } }
        val pow = symbol('^').skip { Parser.pure { x: Double, y: Double -> exp(y * ln(x)) } }

        val pexpr = chainl1(double, add.or { sub }, false)

        assertEquals(Optional.of(Pair(7.0, "abc")), pexpr.parse("7abc"))
        assertEquals(Optional.of(Pair(4.0, "abc")), pexpr.parse(" 7 - 1 - 2 abc"))
        assertEquals(Optional.of(Pair(5.0, "abc")), pexpr.parse(" 7 - 1 + 2 - 3 abc"))
        assertEquals(Optional.empty(), pexpr.parse("abc"))

        assertEquals(Optional.of(Pair(6561.0, "abc")),
            chainr1(double, pow).parse("3 ^ 2 ^ 3 abc").map { (a, out) -> Pair(round(a), out) })
    }
}
