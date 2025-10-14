case class Parser[A](parse : String => Option[(A, String)]) {
  def map[B](f: A => B): Parser[B] = flatMap(a => Parser.pure(f(a)))

  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser[B](inp => parse(inp).flatMap((a, s) => f(a).parse(s)))

  def skip[B](p: => Parser[B]): Parser[B] = flatMap(_ => p)

  def orElse(p: => Parser[A]): Parser[A] = Parser[A](inp => parse(inp).orElse(p.parse(inp)))
}

object Parser:
  def pure[A](a: A): Parser[A] = Parser[A](inp => Some(a, inp))
  def empty[A]: Parser[A] = Parser[A](_ => None)
  def apply[A, B](pf: Parser[A => B], p: => Parser[A]): Parser[B] = pf.flatMap(f => p.map(f))
