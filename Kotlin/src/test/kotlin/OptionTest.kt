import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.util.Optional
import kotlin.math.ln
import kotlin.math.sin
import kotlin.math.sqrt

class OptionTest {

    fun safe_sqrt(x: Double): Optional<Double> {
        return if(x >= 0){ Optional.of(sqrt(x))} else { Optional.empty() }
    }

    fun safe_log(x: Double): Optional<Double>{
        return if(x > 0){ Optional.of(ln(x))} else { Optional.empty() }
    }

    @Test
    fun `test option map` () {
        assertEquals(Optional.of(sin(1.0)), Optional.of(1.0).map(::sin))
        assertEquals(Optional.empty<Double>(), Optional.empty<Double>().map(::sin))
        assertEquals(Optional.of("1"), Optional.of(1).map { i -> i.toString() })
        assertEquals(Optional.empty<String>(), Optional.empty<Int>().map { i -> i.toString() })
    }

    @Test
    fun `test option flatMap` () {
        val nd = Optional.empty<Double>()
        val ns = Optional.empty<String>()

        assertEquals(Optional.of(sqrt(2.0)), safe_sqrt(2.0))
        assertEquals(Optional.of(sqrt(0.0)), safe_sqrt(0.0))
        assertEquals(nd, safe_sqrt(-2.0))

        assertEquals(Optional.of(ln(2.0)), safe_log(2.0))
        assertEquals(nd, safe_log(0.0))
        assertEquals(nd, safe_log(-2.0))

        assertEquals(Optional.of(sqrt(2.0)), Optional.of(2.0).flatMap{ x -> safe_sqrt(x) })
        assertEquals(Optional.of(sqrt(0.0)), Optional.of(0.0).flatMap{ x -> safe_sqrt(x) })
        assertEquals(nd, Optional.of(-2.0).flatMap{ x -> safe_sqrt(x) })

        assertEquals(Optional.of(ln(sqrt(2.0))), Optional.of(2.0).flatMap { x -> safe_sqrt(x) }.flatMap { x -> safe_log(x) })
        assertEquals(nd, Optional.of(0.0).flatMap { x -> safe_sqrt(x) }.flatMap { x -> safe_log(x) })
        assertEquals(nd, Optional.of(-2.0).flatMap{ x -> safe_sqrt(x) }.flatMap { x -> safe_log(x) })

        assertEquals(Optional.of(ln(sqrt(2.0))), safe_sqrt(2.0).flatMap { x -> safe_log(x) })
        assertEquals(nd, safe_sqrt(0.0).flatMap { x -> safe_log(x) })
        assertEquals(nd, safe_sqrt(-2.0).flatMap { x -> safe_log(x) })
        assertEquals(nd, nd.flatMap { x -> safe_log(x) })

        val toString = { i: Int -> if(i % 2 == 0){ Optional.of(i.toString()) } else { Optional.empty() } }

        assertEquals(Optional.of("2"), Optional.of(2).flatMap(toString))
        assertEquals(ns, Optional.of(1).flatMap(toString))
        assertEquals(ns, Optional.empty<Int>().flatMap(toString))
    }
}
