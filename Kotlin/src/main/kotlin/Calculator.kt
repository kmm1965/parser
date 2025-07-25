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
import kotlin.math.asin
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
        defObject("sin") { x: Double -> sin(x) },
        defObject("cos") { x: Double -> cos(x) },
        defObject("asin") { x: Double -> asin(x) },
        defObject("acos") { x: Double -> acos(x) },
        defObject("sinh") { x: Double -> sinh(x) },
        defObject("cosh") { x: Double -> cosh(x) },
        defObject("tan") { x: Double -> tan(x) },
        defObject("log") { x: Double -> ln(x) },
        defObject("log10") { x: Double -> log10(x) },
        defObject("exp") { x: Double -> exp(x) },
        defObject("sqrt") { x: Double -> sqrt(x) },
        defObject("sqr") { x: Double -> x * x }
    ))

    val mLOG2E = 1.4426950408889634   // log2(e)
    val mLOG10E = 0.4342944819032518  // log10(e)
    val mLN2 = 0.6931471805599453     // ln(2)
    val mLN10 = 2.302585092994046     // ln(10)
    val mPI2 = 1.5707963267948966     // pi/2
    val mPI4 = 0.7853981633974483     // pi/4
    val m1PI = 0.3183098861837907     // 1/pi
    val m2PI = 0.6366197723675814     // 2/pi
    val m2SQRTPI = 1.1283791670955126 // 2/sqrt(pi)
    val mSQRT2 = 1.4142135623730951   // sqrt(2)
    val mSQRT12 = 0.7071067811865476  // 1/sqrt(2)

    val const = fold(listOf(
        defObject("E", Math.E),
        defObject("PI", Math.PI),
        defObject("LOG2E", mLOG2E),
        defObject("LOG10E", mLOG10E),
        defObject("LN2", mLN2),
        defObject("LN10", mLN10),
        defObject("PI_2", mPI2),
        defObject("PI_4", mPI4),
        defObject("1_PI", m1PI),
        defObject("2_PI", m2PI),
        defObject("2_SQRTPI", m2SQRTPI),
        defObject("SQRT2", mSQRT2),
        defObject("SQRT1_2", mSQRT12)
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
        return exprInBrackets.orElse{ Parser.apply(func) { exprInBrackets } }.orElse { const }.orElse { double }
    }

    val exprInBrackets: Parser<Double> = between(symbol('('), symbol(')')) { expr() }

    fun calculate(s: String): Option<Pair<Double, String>>{
        return expr().parse(s)
    }
}
