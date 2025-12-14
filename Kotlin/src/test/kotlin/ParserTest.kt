import org.junit.jupiter.api.Assertions.assertEquals
import java.util.Optional
import kotlin.math.sin
import kotlin.test.Test

class ParserTest {
    @Test
    fun `test Parser pure` () {
        assertEquals(Optional.of(Pair(1, "abc")), Parser.pure(1).parse("abc"))
        assertEquals(Optional.of(Pair(1.0, "abc")), Parser.pure(1.0).parse("abc"))
        assertEquals(Optional.of(Pair("1", "abc")), Parser.pure("1").parse("abc"))
    }

    @Test
    fun `test Parser functor` () {
        val fi = { i: Int -> i.toString() }
        val fd = { d: Double -> d.toString() }
        val fs = { s: String -> s.toInt() }

        assertEquals(Optional.of(Pair("1", "abc")), Parser.pure(1).map(fi).parse("abc"))
        assertEquals(Optional.of(Pair("1.0", "abc")), Parser.pure(1.0).map(fd).parse("abc"))
        assertEquals(Optional.of(Pair(1, "abc")), Parser.pure("1").map(fs).parse("abc"))

        assertEquals(Optional.empty<Pair<String, String>>(), Parser.empty<Int>().map(fi).parse("abc"))
        assertEquals(Optional.empty<Pair<String, String>>(), Parser.empty<Double>().map(fd).parse("abc"))
        assertEquals(Optional.empty<Pair<Int, String>>(), Parser.empty<String>().map(fs).parse("abc"))
    }

    @Test
    fun `test Parser applicative` () {
        val psin = Parser.pure { x: Double -> sin(x) }
        val emptyf = Parser.empty<(Double) -> Double>()
        val fd = { Parser.pure(1.0) }
        val nf = { Parser.empty<Double>() }

        assertEquals(Optional.of(Pair(sin(1.0), "abc")), Parser.apply(psin, fd).parse("abc"))
        assertEquals(Optional.empty<Pair<Double, String>>(), Parser.apply(psin, nf).parse("abc"))
        assertEquals(Optional.empty<Pair<Double, String>>(), Parser.apply(emptyf, fd).parse("abc"))
        assertEquals(Optional.empty<Pair<Double, String>>(), Parser.apply(emptyf, nf).parse("abc"))
    }

    @Test
    fun `test Parser monad` () {
        val i1 = Parser.pure(1)
        val iempty = Parser.empty<Int>()
        val eat = { x: Int -> Parser { inp -> Optional.of(Pair(x.toString() + inp, "")) } }
        val cancel = { _: Int -> Parser { _ -> Optional.empty<Pair<String, String>>() } }
        val nss = Optional.empty<Pair<String, String>>()

        assertEquals(Optional.of(Pair("1abc", "")), i1.flatMap(eat).parse("abc"))
        assertEquals(nss, i1.flatMap(cancel).parse("abc"))
        assertEquals(nss, iempty.flatMap(eat).parse("abc"))
        assertEquals(nss, iempty.flatMap(cancel).parse("abc"))
    }
}
