import SomeParsers.Companion.between
import SomeParsers.Companion.chainl1
import SomeParsers.Companion.chainr1
import SomeParsers.Companion.double
import SomeParsers.Companion.identifier
import SomeParsers.Companion.symbol
import SomeParsers.Companion.usign
import java.util.Optional
import kotlin.collections.listOf
import kotlin.math.acos
import kotlin.math.acosh
import kotlin.math.asin
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

class Calculator {
    fun op2(c: Char, f: (Double, Double) -> Double) : Parser<(Double, Double) -> Double>{
        return symbol(c).skip{ Parser.pure(f) }
    }

    val add = op2('+') { x, y -> x + y }
    val sub = op2('-') { x, y -> x - y }
    val mul = op2('*') { x, y -> x * y }
    val div = op2('/') { x, y -> x / y }
    val pow = op2('^') { x, y -> exp(y * ln(x)) }

    fun <A> fold(parsers: List<Parser<A> >): Parser<A>{
        return parsers.stream().reduce(Parser.empty()) { p1, p2 -> p1.or { p2 } }
    }

    fun <A> guard(b: Boolean, value: A): Parser<A>{
        return if(b) { Parser.pure(value) } else { Parser.empty() }
    }

    val funcs = identifier.flatMap { n -> fold(listOf(
        guard(n == "sin")  { x: Double -> sin(x) },
        guard(n == "cos")   { x: Double -> cos(x) },
        guard(n == "asin")  { x: Double -> asin(x) },
        guard(n == "acos")  { x: Double -> acos(x) },
        guard(n == "sinh")  { x: Double -> sinh(x) },
        guard(n == "cosh")  { x: Double -> cosh(x) },
        guard(n == "asinh") { x: Double -> asinh(x) },
        guard(n == "acosh") { x: Double -> acosh(x) },
        guard(n == "tan")   { x: Double -> tan(x) },
        guard(n == "log")   { x: Double -> ln(x) },
        guard(n == "log10") { x: Double -> log10(x) },
        guard(n == "exp")   { x: Double -> exp(x) },
        guard(n == "sqrt")  { x: Double -> sqrt(x) },
        guard(n == "sqr")   { x: Double -> x * x }
    )) }

    val consts = identifier.flatMap { n -> fold(listOf(
        guard(n == "E",        Math.E),
        guard(n == "PI",       Math.PI),
        guard(n == "LOG2E",    1.4426950408889634),  // log2(e)
        guard(n == "LOG10E",   0.4342944819032518), // log10(e)
        guard(n == "LN2",      0.6931471805599453), // ln(2)
        guard(n == "LN10",     2.302585092994046),  // ln(10)
        guard(n == "PI_2",     1.5707963267948966),  // pi/2
        guard(n == "PI_4",     0.7853981633974483), // pi/4
        guard(n == "1_PI",     0.3183098861837907), // 1/pi
        guard(n == "2_PI",     0.6366197723675814), // 2/pi
        guard(n == "2_SQRTPI", 1.1283791670955126),  // 2/sqrt(pi)
        guard(n == "SQRT2",    1.4142135623730951),  // sqrt(2)
        guard(n == "SQRT1_2",  0.7071067811865476)  // 1/sqrt(2)
    )) }

    fun expr (): Parser<Double> {
        return usign.flatMap { sgn -> chainl1(term(), add.or { sub }, sgn == "-") }
    }

    fun term(): Parser<Double>{
        return chainl1(factor(), mul.or { div }, false)
    }

    fun factor(): Parser<Double>{
        return chainr1(factor0(), pow)
    }

    fun factor0(): Parser<Double> {
        return exprInBrackets
            .or { Parser.apply(funcs) { exprInBrackets } }
            .or { consts }
            .or { double }
    }

    val exprInBrackets: Parser<Double> = between(symbol('('), symbol(')')) { expr() }

    fun calculate(s: String): Optional<Pair<Double, String>> {
        return expr().parse(s)
    }
}
