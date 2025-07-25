import arrow.core.Some
import kotlin.math.round
import kotlin.math.sin
import kotlin.test.Test
import kotlin.test.assertEquals

class CalculatorTest {
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