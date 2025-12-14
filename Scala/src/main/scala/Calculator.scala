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

  private def guard[A](b: Boolean, value: A): Parser[A] = if b then pure(value) else empty

  private val funcs: Parser[Double => Double] = identifier.flatMap(n => fold(List(
    guard(n.equals("sin"),   sin),
    guard(n.equals("cos"),   cos),
    guard(n.equals("asin"),  asin),
    guard(n.equals("acos"),  acos),
    guard(n.equals("sinh"),  sinh),
    guard(n.equals("cosh"),  cosh),
    guard(n.equals("tan"),   tan),
    guard(n.equals("log"),   log),
    guard(n.equals("log10"), log10),
    guard(n.equals("exp"),   exp),
    guard(n.equals("sqrt"),  sqrt),
    guard(n.equals("sqr"),   (x: Double) => x * x)
  )))

  private val consts = identifier.flatMap(n => fold(List(
    guard(n.equals("E"),        Math.E),
    guard(n.equals("PI"),       Math.PI),
    guard(n.equals("LOG2E"),    1.44269504088896340736),  // log2(e)
    guard(n.equals("LOG10E"),   0.434294481903251827651), // log10(e)
    guard(n.equals("LN2"),      0.693147180559945309417), // ln(2)
    guard(n.equals("LN10"),     2.30258509299404568402),  // ln(10)
    guard(n.equals("PI_2"),     1.57079632679489661923),  // pi/2
    guard(n.equals("PI_4"),     0.785398163397448309616), // pi/4
    guard(n.equals("1_PI"),     0.318309886183790671538), // 1/pi
    guard(n.equals("2_PI"),     0.636619772367581343076), // 2/pi
    guard(n.equals("2_SQRTPI"), 1.12837916709551257390),  // 2/sqrt(pi)
    guard(n.equals("SQRT2"),    1.41421356237309504880),  // sqrt(2)
    guard(n.equals("SQRT1_2"),  0.707106781186547524401)  // 1/sqrt(2)
  )))

  private val expr: () => Parser[Double] = () => usign.flatMap(sgn => chainl1(term(), add.orElse(sub), sgn.equals("-")))

  private val term: () => Parser[Double] = () => chainl1(factor(), mul.orElse(div), false)

  private val factor: () => Parser[Double] = () => chainr1(factor0(), pow)

  private val factor0: () => Parser[Double] = () =>
    expr_in_brackets
      .orElse(apply(funcs, expr_in_brackets))
      .orElse(consts)
      .orElse(double)

  private val expr_in_brackets: Parser[Double] = between(symbol('('), symbol(')'), expr())

  def calculate(s: String): Option[(Double, String)] = expr().parse(s)
}
