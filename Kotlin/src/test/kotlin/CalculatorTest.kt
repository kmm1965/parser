import arrow.core.Some
import kotlin.math.round
import kotlin.math.acos
import kotlin.math.asin
import kotlin.math.acosh
import kotlin.math.asinh
import kotlin.math.cos
import kotlin.math.cosh
import kotlin.math.exp
import kotlin.math.ln
import kotlin.math.log10
import kotlin.math.sin
import kotlin.math.sinh
import kotlin.math.sqrt
import kotlin.math.tan
import kotlin.test.Test
import kotlin.test.assertEquals

class CalculatorTest {
    @Test
    fun `test funcs` () {
        val calc = Calculator()

        assertEquals(Some(Pair(sin(2.0), "")), calc.calculate("sin(2.0)"))
        assertEquals(Some(Pair(cos(2.0), "")), calc.calculate("cos(2.0)"))
        assertEquals(Some(Pair(asin(0.5), "")), calc.calculate("asin(0.5)"))
        assertEquals(Some(Pair(acos(0.5), "")), calc.calculate("acos(0.5)"))
        assertEquals(Some(Pair(sinh(2.0), "")), calc.calculate("sinh(2.0)"))
        assertEquals(Some(Pair(cosh(2.0), "")), calc.calculate("cosh(2.0)"))
        assertEquals(Some(Pair(asinh(2.0), "")), calc.calculate("asinh(2.0)"))
        assertEquals(Some(Pair(acosh(2.0), "")), calc.calculate("acosh(2.0)"))
        assertEquals(Some(Pair(tan(2.0), "")), calc.calculate("tan(2.0)"))
        assertEquals(Some(Pair(ln(2.0), "")), calc.calculate("log(2.0)"))
        assertEquals(Some(Pair(log10(2.0), "")), calc.calculate("log10(2.0)"))
        assertEquals(Some(Pair(exp(2.0), "")), calc.calculate("exp(2.0)"))
        assertEquals(Some(Pair(sqrt(2.0), "")), calc.calculate("sqrt(2.0)"))
        assertEquals(Some(Pair(4.0, "")), calc.calculate("sqr(2.0)"))
    }

    @Test
    fun `test constsgty` () {
        val calc = Calculator()

        assertEquals(Some(Pair(Math.E, "")), calc.calculate("E"))
        assertEquals(Some(Pair(1 / ln(2.0), "")), calc.calculate("LOG2E"))
        assertEquals(Some(Pair(0.4342944819032518, "")), calc.calculate("LOG10E"))
        assertEquals(Some(Pair(ln(2.0), "")), calc.calculate("LN2"))
        assertEquals(Some(Pair(ln(10.0), "")), calc.calculate("LN10"))
        assertEquals(Some(Pair(Math.PI, "")), calc.calculate("PI"))
        assertEquals(Some(Pair(Math.PI / 2, "")), calc.calculate("PI_2"))
        assertEquals(Some(Pair(Math.PI / 4, "")), calc.calculate("PI_4"))
        assertEquals(Some(Pair(1 / Math.PI, "")), calc.calculate("1_PI"))
        assertEquals(Some(Pair(2 / Math.PI, "")), calc.calculate("2_PI"))
        assertEquals(Some(Pair(2 / sqrt(Math.PI), "")), calc.calculate("2_SQRTPI"))
        assertEquals(Some(Pair(sqrt(2.0), "")), calc.calculate("SQRT2"))
        assertEquals(Some(Pair(sqrt(0.5), "")), calc.calculate("SQRT1_2"))
    }

    @Test
    fun `test calculator` () {
        val calc = Calculator()

        assertEquals(Some(Pair(68.0, "")), calc.calculate("72 - 7 - (1 - 2) * 3"))
        assertEquals(Some(Pair(-8.889, "")), calc.calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"))
        assertEquals(Some(Pair(6561.0, "")), calc.calculate("3^(1+1)^3").map { (x, out) -> Pair(round(x), out) })
        assertEquals(Some(Pair(sin(2.0), "")), calc.calculate("sin(1+1)"))
        assertEquals(Some(Pair(-0.3634085731426532, "")), calc.calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"))
        assertEquals(Some(Pair(25.0, "")), calc.calculate("sqr(2 + 3)"))
        assertEquals(Some(Pair(-0.7071067811865475, "")), calc.calculate("sin(-PI/4)"))
        assertEquals(Some(Pair(23.140692632779267, "")), calc.calculate(" E ^ PI"))
        assertEquals(Some(Pair(22.45915771836104, "")), calc.calculate(" PI ^ E"))
    }
}