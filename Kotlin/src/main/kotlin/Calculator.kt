import SomeParsers.Companion.between
import SomeParsers.Companion.chainl1
import SomeParsers.Companion.chainr1
import SomeParsers.Companion.double
import SomeParsers.Companion.name
import SomeParsers.Companion.symbol
import SomeParsers.Companion.token
import SomeParsers.Companion.usign
import arrow.core.Option
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
        var p0 = Parser.empty<A>()
        for(p in parsers){
            p0 = p0.orElse{ p }
        }
        return token(p0)
    }

    fun <A> defObject(n: String, value: A): Parser<A>{
        return name(n).skip { Parser.pure(value) }
    }

    val func = fold(listOf(
        defObject("sin")   { x: Double -> sin(x) },
        defObject("cos")   { x: Double -> cos(x) },
        defObject("asin")  { x: Double -> asin(x) },
        defObject("acos")  { x: Double -> acos(x) },
        defObject("sinh")  { x: Double -> sinh(x) },
        defObject("cosh")  { x: Double -> cosh(x) },
        defObject("asinh") { x: Double -> asinh(x) },
        defObject("acosh") { x: Double -> acosh(x) },
        defObject("tan")   { x: Double -> tan(x) },
        defObject("log")   { x: Double -> ln(x) },
        defObject("log10") { x: Double -> log10(x) },
        defObject("exp")   { x: Double -> exp(x) },
        defObject("sqrt")  { x: Double -> sqrt(x) },
        defObject("sqr")   { x: Double -> x * x }
    ))

    val const = fold(listOf(
        defObject("E",        Math.E),
        defObject("PI",       Math.PI),
        defObject("LOG2E",    1.44269504088896340736),  // log2(e)
        defObject("LOG10E",   0.434294481903251827651), // log10(e)
        defObject("LN2",      0.693147180559945309417), // ln(2)
        defObject("LN10",     2.30258509299404568402),  // ln(10)
        defObject("PI_2",     1.57079632679489661923),  // pi/2
        defObject("PI_4",     0.785398163397448309616), // pi/4
        defObject("1_PI",     0.318309886183790671538), // 1/pi
        defObject("2_PI",     0.636619772367581343076), // 2/pi
        defObject("2_SQRTPI", 1.12837916709551257390),  // 2/sqrt(pi)
        defObject("SQRT2",    1.41421356237309504880),  // sqrt(2)
        defObject("SQRT1_2",  0.707106781186547524401)  // 1/sqrt(2)
    ))

    fun expr (): Parser<Double> {
        return usign.flatMap { sgn -> chainl1(term(), add.orElse { sub }, sgn == "-") }
    }

    fun term(): Parser<Double>{
        return chainl1(factor(), mul.orElse { div }, false)
    }

    fun factor(): Parser<Double>{
        return chainr1(factor0(), pow)
    }

    fun factor0(): Parser<Double> {
        return exprInBrackets
            .orElse { Parser.apply(func) { exprInBrackets } }
            .orElse { const }
            .orElse { double }
    }

    val exprInBrackets: Parser<Double> = between(symbol('('), symbol(')')) { expr() }

    fun calculate(s: String): Option<Pair<Double, String>>{
        return expr().parse(s)
    }
}
