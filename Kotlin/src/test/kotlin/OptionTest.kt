import arrow.core.Option
import arrow.core.Some
import arrow.core.none
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import kotlin.math.ln
import kotlin.math.sin
import kotlin.math.sqrt

class OptionTest {

    fun safe_sqrt(x: Double): Option<Double>{
        return if(x >= 0){ Some(sqrt(x))} else { none() }
    }

    fun safe_log(x: Double): Option<Double>{
        return if(x > 0){ Some(ln(x))} else { none() }
    }

    @Test
    fun `test option map` () {
        assertEquals(Some(sin(1.0)), Some(1.0).map(::sin))
        assertEquals(none<Double>(), none<Double>().map(::sin))
        assertEquals(Some("1"), Some(1).map { i -> i.toString() })
        assertEquals(none<String>(), none<Int>().map { i -> i.toString() })
    }

    @Test
    fun `test option flatMap` () {
        val nd = none<Double>()
        val ns = none<String>()

        assertEquals(Some(sqrt(2.0)), safe_sqrt(2.0))
        assertEquals(Some(sqrt(0.0)), safe_sqrt(0.0))
        assertEquals(nd, safe_sqrt(-2.0))

        assertEquals(Some(ln(2.0)), safe_log(2.0))
        assertEquals(nd, safe_log(0.0))
        assertEquals(nd, safe_log(-2.0))

        assertEquals(Some(sqrt(2.0)), Some(2.0).flatMap{ x -> safe_sqrt(x) })
        assertEquals(Some(sqrt(0.0)), Some(0.0).flatMap{ x -> safe_sqrt(x) })
        assertEquals(nd, Some(-2.0).flatMap{ x -> safe_sqrt(x) })

        assertEquals(Some(ln(sqrt(2.0))), Some(2.0).flatMap { x -> safe_sqrt(x) }.flatMap { x -> safe_log(x) })
        assertEquals(nd, Some(0.0).flatMap { x -> safe_sqrt(x) }.flatMap { x -> safe_log(x) })
        assertEquals(nd, Some(-2.0).flatMap{ x -> safe_sqrt(x) }.flatMap { x -> safe_log(x) })

        assertEquals(Some(ln(sqrt(2.0))), safe_sqrt(2.0).flatMap { x -> safe_log(x) })
        assertEquals(nd, safe_sqrt(0.0).flatMap { x -> safe_log(x) })
        assertEquals(nd, safe_sqrt(-2.0).flatMap { x -> safe_log(x) })
        assertEquals(nd, nd.flatMap { x -> safe_log(x) })

        val toString = { i: Int -> if(i % 2 == 0){ Some(i.toString()) } else { none() } }

        assertEquals(Some("2"), Some(2).flatMap(toString))
        assertEquals(ns, Some(1).flatMap(toString))
        assertEquals(ns, none<Int>().flatMap(toString))
    }
}
