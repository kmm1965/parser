import org.scalatest.funsuite.AnyFunSuite

import scala.math.*
import Parser.*
import SomeParsers.*
import Calculator.calculate

class ParserTest extends AnyFunSuite {

  private def test_maybe_map(): Unit = {
    assert(Some(1.0).map(sin) === Some(sin(1.0)))
    assert(None.map(sin) === None)
    assert(Some(1).map(Integer.toString) === Some("1"))
    assert(None.map(Integer.toString) === None)
  }

  private def safe_sqrt (x: Double) = if x >= 0 then Some(sqrt(x)) else None
  private def safe_log  (x: Double) = if x >  0 then Some(log(x))  else None

  private def to_string(x: Int): Option[String] = if x % 2 == 0 then Some(x.toString) else None

  private def test_maybe_flat_map(): Unit = {
    assert(safe_sqrt(2.0) === Some(sqrt(2.0)))
    assert(safe_sqrt(0.0) === Some(sqrt(0.0)))
    assert(safe_sqrt(-2.0) === None)

    assert(safe_log(2.0) === Some(log(2.0)))
    assert(safe_log(0.0) === None)
    assert(safe_log(-2.0) === None)

    assert(Some(2.0).flatMap(safe_sqrt) === Some(sqrt(2.0)))
    assert(Some(0.0).flatMap(safe_sqrt) === Some(sqrt(0.0)))
    assert(None.flatMap(safe_sqrt) === None)

    assert(Some(2.0).flatMap(safe_sqrt).flatMap(safe_log) === Some(log(sqrt(2.0))))
    assert(Some(-2.0).flatMap(safe_sqrt).flatMap(safe_log) === None)
    assert(Some(0.0).flatMap(safe_sqrt).flatMap(safe_log) === None)

    assert(safe_sqrt(2.0).flatMap(safe_log) === Some(log(sqrt(2.0))))
    assert(safe_sqrt(-2.0).flatMap(safe_log) === None)
    assert(safe_sqrt(0.0).flatMap(safe_log) === None)
    assert(None.flatMap(safe_log) === None)

    assert(Some(2).flatMap(to_string) === Some("2"))
    assert(Some(1).flatMap(to_string) === None)
    assert(None.flatMap(to_string) === None)
  }

  private def test_maybe(): Unit = {
    test_maybe_map()
    test_maybe_flat_map()
  }

  private def test_parser_pure(): Unit = {
    pure(1).parse("abc") === Some((1, "abc"))
    pure(1.0).parse("abc") === Some((1.0, "abc"))
    pure("1").parse("abc") === Some(("1", "abc"))
  }

  private def test_parser_functor(): Unit = {
    val fi = (i: Int) => i.toString
    val fd = (d: Double) => d.toString
    val fs = (s: String) => s.toInt

    assert(pure(1).map(fi).parse("abc") === Some(("1", "abc")))
    assert(pure(1.0).map(fd).parse("abc") === Some(("1.0", "abc")))
    assert(pure("1").map(fs).parse("abc") === Some((1, "abc")))

    assert(empty[Int].map(fi).parse("abc") === None)
    assert(empty[Double].map(fd).parse("abc") === None)
    assert(empty[String].map(fs).parse("abc") === None)
  }

  private def test_parser_applicative(): Unit = {
    val psin = pure(sin)
    val emptyf = empty[Double => Double]
    val fd = pure(1.0)
    val nf = empty[Double]

    assert(apply(psin, fd).parse("abc") === Some((sin(1.0), "abc")))
    assert(apply(psin, nf).parse("abc") === None)
    assert(apply(emptyf, fd).parse("abc") === None)
    assert(apply(emptyf, nf).parse("abc") === None)
  }

  private def test_parser_monad(): Unit = {
    val i1 = pure(1)
    val iempty = empty[Int]
    val eat = (x: Int) => Parser(inp => Some((x.toString + inp, "")))
    val cancel = (_: Int) => Parser(_ => None)

    assert(i1.flatMap(eat).parse("abc") === Some(("1abc", "")))
    assert(i1.flatMap(cancel).parse("abc") === None)
    assert(iempty.flatMap(eat).parse("abc") === None)
    assert(i1.flatMap(cancel).parse("abc") === None)
  }

  private def test_parser(): Unit = {
    test_parser_pure()
    test_parser_functor()
    test_parser_applicative()
    test_parser_monad()
  }

  private def test_anyChar(): Unit = {
    assert(anyChar.parse("abc") === Some(('a', "bc")))
    assert(anyChar.parse("") === None)
  }

  private def test_satisfy(): Unit = {
    assert(satisfy(_ == 'a').parse("abc") === Some(('a', "bc")))
    assert(satisfy(_ == 'z').parse("abc") === None)
  }

  private def test_char(): Unit = {
    assert(char('a').parse("abc") === Some(('a', "bc")))
    assert(char('z').parse("abc") === None)
  }

  private def test_empty_string(): Unit = {
    assert(empty_string.parse("abc") === Some(("", "abc")))
  }

  private def test_optional(): Unit = {
    assert(optional_c(char('1')).parse("1234") === Some(("1", "234")))
    assert(optional_c(char('1')).parse("abc") === Some(("", "abc")))
  }

  private def test_spaces(): Unit = {
    assert(spaces.parse("abc") === Some(("", "abc")))
    assert(spaces.parse("  abc") === Some(("  ", "abc")))
  }

  private def test_symbol(): Unit = {
    assert(symbol('+').parse(" + abc") === Some(('+', "abc")))
    assert(symbol('+').parse("abc") === None)
  }

  private def test_alnum(): Unit = {
    assert(alnum.parse("123abc") === Some(('1', "23abc")))
    assert(alnum.parse("_123abc") === Some(('_', "123abc")))
    assert(alnum.parse("!@#$") === None)
  }

  private def test_name(): Unit = {
    val psin = name("sin")

    assert(psin.parse(" sin ") === Some(("sin", "")))
    assert(psin.parse(" sin (1.)") === Some(("sin", "(1.)")))
    assert(psin.parse("sinabc") === None)
  }

  private def test_sign(): Unit = {
    assert(sign.parse("abc") === Some(("", "abc")))
    assert(sign.parse("+abc") === Some(("+", "abc")))
    assert(sign.parse("-abc") === Some(("-", "abc")))

    assert(usign.parse("abc") === Some(("", "abc")))
    assert(usign.parse(" + abc") === Some(("+", "abc")))
    assert(usign.parse(" - abc") === Some(("-", "abc")))
  }

  private def test_digits(): Unit = {
    assert(digits.parse("123abc") === Some(("123", "abc")))
    assert(digits.parse("123   abc") === Some(("123", "   abc")))
    assert(digits.parse("abc") === Some(("", "abc")))
  }

  private def test_double(): Unit = {
    assert(double.parse("1 abc") === Some((1.0, "abc")))
    assert(double.parse("1. abc") === Some((1.0, "abc")))
    assert(double.parse("1.23 abc") === Some((1.23, "abc")))
    assert(double.parse("-1.23 abc") === Some((-1.23, "abc")))
    assert(double.parse(".23 abc") === Some((0.23, "abc")))
    assert(double.parse(" + 1.23 abc") === None)
    assert(double.parse("1.23e10abc") === Some((1.23e10, "abc")))
    assert(double.parse("1.23e-10abc") === Some((1.23e-10, "abc")))
    assert(double.parse("abc") === None)
  }

  private def test_between(): Unit = {
    val expr = between(symbol('('), symbol(')'), double)

    assert(expr.parse(" ( 123 ) abc") === Some((123.0, "abc")))
    assert(expr.parse(" ( 123 abc") === None)
    assert(expr.parse(" 123 ) abc") === None)
  }

  private def test_chainlr1(): Unit = {
    val add = symbol('+').skip(pure((x: Double, y: Double) => x + y))
    val sub = symbol('-').skip(pure((x: Double, y: Double) => x - y))
    val pexpr = chainl1(double, add.orElse(sub), false)
    val pow = symbol('^').skip(pure((x: Double, y: Double) => exp(y * log(x))))

    assert(pexpr.parse("7abc") === Some((7.0, "abc")))
    assert(pexpr.parse(" 7 - 1 - 2 abc") === Some((4.0, "abc")))
    assert(pexpr.parse(" 7 - 1 + 2 - 3 abc") === Some((5.0, "abc")))
    assert(pexpr.parse("abc") === None)

    assert(chainr1(double, pow).parse("3 ^ 2 ^ 3 abc").map((x, s) => (rint(x), s)) === Some((6561.0, "abc")))
  }

  private def test_calculator(): Unit = {
    assert(calculate("72 - 7 - (1 - 2) * 3") === Some((68.0, "")))
    assert(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") === Some((-8.889, "")))
    assert(calculate("3^(1+1)^3").map((x, s) => (rint(x), s)) === Some((6561.0, "")))
    assert(calculate("sin(1+1)") === Some((sin(2.0), "")))
    assert(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )") === Some((-0.3634085731426532, "")))
    assert(calculate("sqr(2 + 3)") === Some((25.0, "")))
    assert(calculate("sin(-PI/4)") === Some((-0.7071067811865475, "")))
    assert(calculate(" E ^ PI") === Some((23.140692632779267, "")))
    assert(calculate(" PI ^ E") === Some((22.45915771836104, "")))
  }

  private def test_some_parasers(): Unit = {
    test_anyChar()
    test_satisfy()
    test_char()
    test_empty_string()
    test_optional()
    test_spaces()
    test_symbol()
    test_alnum()
    test_name()
    test_sign()
    test_digits()
    test_double()
    test_between()
    test_chainlr1()
  }

  test("Parser") {
    test_maybe()
    test_parser()
    test_some_parasers()
    test_calculator()
  }
}
