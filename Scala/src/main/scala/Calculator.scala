import Parser.*
import SomeParsers.*

import scala.math.*

object Calculator {
  private def op2(c: Char, f: (Double, Double) => Double): Parser[(Double, Double) => Double] =
    symbol(c).skip(pure(f))

  private val add = op2('+', (x, y) => x + y)
  private val sub = op2('-', (x, y) => x - y)
  private val mul = op2('*', (x, y) => x * y)
  private val div = op2('/', (x, y) => x / y)
  private val pow = op2('^', (x, y) => exp(y * log(x)))

  private def fold[A](parsers: List[Parser[A]]): Parser[A] = {
    var p0 = empty[A]
    parsers.foreach(p => p0 = p0.orElse(p))
    token(p0)
  }

  private def def_object[A](n: String, value: A): Parser[A] = name(n).skip(pure(value))

  private val func: Parser[Double => Double] = fold(List(
    def_object("sin",   sin),
    def_object("cos",   cos),
    def_object("asin",  asin),
    def_object("acos",  acos),
    def_object("sinh",  sinh),
    def_object("cosh",  cosh),
    def_object("tan",   tan),
    def_object("log",   log),
    def_object("log10", log10),
    def_object("exp", exp),
    def_object("sqrt", sqrt),
    def_object("sqr", (x: Double) => x * x)
  ))

  private val M_LOG2E = 1.44269504088896340736    // log2(e)
  private val M_LOG10E = 0.434294481903251827651  // log10(e)
  private val M_LN2 = 0.693147180559945309417     // ln(2)
  private val M_LN10 = 2.30258509299404568402     // ln(10)
  private val M_PI_2 = 1.57079632679489661923     // pi/2
  private val M_PI_4 = 0.785398163397448309616    // pi/4
  private val M_1_PI = 0.318309886183790671538    // 1/pi
  private val M_2_PI = 0.636619772367581343076    // 2/pi
  private val M_2_SQRTPI = 1.12837916709551257390 // 2/sqrt(pi)
  private val M_SQRT2 = 1.41421356237309504880    // sqrt(2)
  private val M_SQRT1_2 = 0.707106781186547524401 // 1/sqrt(2)

  private val _const = fold(List(
    def_object("E", Math.E),
    def_object("PI", Math.PI),
    def_object("LOG2E", M_LOG2E),
    def_object("LOG10E", M_LOG10E),
    def_object("LN2", M_LN2),
    def_object("LN10", M_LN10),
    def_object("PI_2", M_PI_2),
    def_object("PI_4", M_PI_4),
    def_object("1_PI", M_1_PI),
    def_object("2_PI", M_2_PI),
    def_object("2_SQRTPI", M_2_SQRTPI),
    def_object("SQRT2", M_SQRT2),
    def_object("SQRT1_2", M_SQRT1_2)
  ))

  private val expr: () => Parser[Double] = () => usign.flatMap(sgn => chainl1(term(), add.orElse(sub), sgn.equals("-")))
  private val term: () => Parser[Double] = () => chainl1(factor(), mul.orElse(div), false)
  private val factor: () => Parser[Double] = () => chainr1(factor0(), pow)
  private val factor0: () => Parser[Double] = () =>
    expr_in_brackets.orElse(apply(func, expr_in_brackets)).orElse(_const).orElse(double)
  private val expr_in_brackets: Parser[Double] = between(symbol('('), symbol(')'), expr())

  def calculate(s: String) = expr().parse(s)
}
