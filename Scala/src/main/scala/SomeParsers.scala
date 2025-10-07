import Parser.*

object SomeParsers {
  val anyChar: Parser[Char] = Parser[Char](inp => if inp.isEmpty then None else Some(inp.charAt(0), inp.substring(1)))

  def satisfy(pred: Char => Boolean): Parser[Char] = anyChar.flatMap(c => if pred(c) then pure(c) else empty[Char])

  def char(c: Char): Parser[Char] = satisfy(_ == c)

  val empty_string: Parser[String] = pure("")

  def between[A, O, C](open: Parser[O], close: Parser[C], p: => Parser[A]): Parser[A] =
    open.flatMap(_ => p.flatMap(x => close.flatMap(_ => pure(x))))

  private def some(p: Parser[Char]): Parser[String] = apply(p.map(c => (s: String) => c.toString + s), many(p))

  private def many(p: Parser[Char]): Parser[String] = some(p).orElse(empty_string)

  val spaces: Parser[String] = many(satisfy(Character.isWhitespace))

  def token[A](p: Parser[A]): Parser[A] = between(spaces, spaces, p)

  def symbol(c: Char): Parser[Char] = token(char(c))

  val alnum: Parser[Char] = satisfy(c => Character.isLetterOrDigit(c) || c == '_')

  def name(n: String): Parser[String] = token(some(alnum).flatMap(s => if s.equals(n) then pure(n) else empty))

  private def optional_s(p: Parser[String]): Parser[String] = p.orElse(empty_string)

  def optional_c(p: Parser[Char]): Parser[String] = optional_s(p.map(_.toString))

  val digit: Parser[Char] = satisfy(Character.isDigit)

  val digits: Parser[String] = many(digit)

  val sign: Parser[String] = optional_c(char('+').orElse(char('-')))

  val usign: Parser[String] = optional_c(symbol('+').orElse(symbol('-')))

  val double: Parser[Double] = token(sign.flatMap(
    sign_part => digits.flatMap(
    int_part => optional_s(char('.').skip(digits)).flatMap(
    frac_part => optional_s(char('e').orElse(char('E')).skip(sign).flatMap(
      exp_sign => some(digit).flatMap(
      exp_digits => Parser.pure(exp_sign + exp_digits)))).flatMap(
    exp_part => if int_part.nonEmpty || frac_part.nonEmpty then
      Parser.pure((sign_part + int_part +
        (if frac_part.nonEmpty then "." + frac_part else "") +
        (if exp_part.nonEmpty then "e" + exp_part else "")).toDouble)
      else empty)))))

  private def rest[A](p: => Parser[A], ff: A => Parser[A], op: Parser[(A, A) => A], a: A): Parser[A] =
    op.flatMap(f => p.flatMap(b => ff(f(a, b)))).orElse(pure(a))

  private def rest_l[A](p: Parser[A], op: Parser[(A, A) => A])(a: A): Parser[A] = rest(p, rest_l(p, op), op, a)

  private def rest_r[A](p: Parser[A], op: Parser[(A, A) => A])(a: A): Parser[A] = rest(chainr1(p, op), pure, op, a)

  def chainl1(p: Parser[Double], op: Parser[(Double, Double) => Double], negate_first: Boolean): Parser[Double] =
    p.flatMap(a => rest_l(p, op)(if negate_first then -a else a))

  def chainr1[A](p: Parser[A], op: Parser[(A, A) => A]): Parser[A] = p.flatMap(rest_r(p, op))
}
