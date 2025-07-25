import arrow.core.Some
import arrow.core.none
import org.junit.jupiter.api.Assertions.assertEquals
import kotlin.math.sin
import kotlin.test.Test

class ParserTest {
    @Test
    fun `test Parser pure` () {
        assertEquals(Some(Pair(1, "abc")), Parser.pure(1).parse("abc"))
        assertEquals(Some(Pair(1.0, "abc")), Parser.pure(1.0).parse("abc"))
        assertEquals(Some(Pair("1", "abc")), Parser.pure("1").parse("abc"))
    }

    @Test
    fun `test Parser functor` () {
        val fi = { i: Int -> i.toString() }
        val fd = { d: Double -> d.toString() }
        val fs = { s: String -> s.toInt() }

        assertEquals(Some(Pair("1", "abc")), Parser.pure(1).map(fi).parse("abc"))
        assertEquals(Some(Pair("1.0", "abc")), Parser.pure(1.0).map(fd).parse("abc"))
        assertEquals(Some(Pair(1, "abc")), Parser.pure("1").map(fs).parse("abc"))

        assertEquals(none<Pair<String, String>>(), Parser.empty<Int>().map(fi).parse("abc"))
        assertEquals(none<Pair<String, String>>(), Parser.empty<Double>().map(fd).parse("abc"))
        assertEquals(none<Pair<Int, String>>(), Parser.empty<String>().map(fs).parse("abc"))
    }

    @Test
    fun `test Parser applicative` () {
        val psin = Parser.pure { x: Double -> sin(x) }
        val emptyf = Parser.empty<(Double) -> Double>()
        val fd = { Parser.pure(1.0) }
        val nf = { Parser.empty<Double>() }

        assertEquals(Some(Pair(sin(1.0), "abc")), Parser.apply(psin, fd).parse("abc"))
        assertEquals(none<Pair<Double, String>>(), Parser.apply(psin, nf).parse("abc"))
        assertEquals(none<Pair<Double, String>>(), Parser.apply(emptyf, fd).parse("abc"))
        assertEquals(none<Pair<Double, String>>(), Parser.apply(emptyf, nf).parse("abc"))
    }

    @Test
    fun `test Parser monad` () {
        val i1 = Parser.pure(1)
        val iempty = Parser.empty<Int>()
        val eat = { x: Int -> Parser { inp -> Some(Pair(x.toString() + inp, "")) } }
        val cancel = { _: Int -> Parser { _ -> none<Pair<String, String>>() } }
        val nss = none<Pair<String, String>>()

        assertEquals(Some(Pair("1abc", "")), i1.flatMap(eat).parse("abc"))
        assertEquals(nss, i1.flatMap(cancel).parse("abc"))
        assertEquals(nss, iempty.flatMap(eat).parse("abc"))
        assertEquals(nss, iempty.flatMap(cancel).parse("abc"))
    }
}
