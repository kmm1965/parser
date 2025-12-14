use "../src/some_parsers.sml";

open SomeParsers
open Parser

structure Calculator =
struct

  fun op2(c: char, f: real * real -> real) =
    skip(symbol(c), fn () => pure(f));

  val add  = op2(#"+", fn (x, y) => x + y);
  val sub  = op2(#"-", fn (x, y) => x - y);
  val mul  = op2(#"*", fn (x, y) => x * y);
  val div_ = op2(#"/", fn (x, y) => x / y);
  val pow  = op2(#"^", fn (x, y) => Math.exp(y * Math.ln(x)));

  fun fold(parsers: ('a Parser) list) =
    token(List.foldl(fn (p, q) => or_else(p, fn () => q)) empty parsers);

  fun guard(b: bool, value: 'a) =
    if b then pure(value) else empty;

  val funcs = flat_map(identifier, fn(n) => fold([
    guard(n = "sin",   fn (x) => Math.sin(x)),
    guard(n = "cos",   fn (x) => Math.cos(x)),
    guard(n = "asin",  fn (x) => Math.asin(x)),
    guard(n = "acos",  fn (x) => Math.acos(x)),
    guard(n = "sinh",  fn (x) => Math.sinh(x)),
    guard(n = "cosh",  fn (x) => Math.cosh(x)),
    guard(n = "tan",   fn (x) => Math.tan(x)),
    guard(n = "log",   fn (x) => Math.ln(x)),
    guard(n = "log10", fn (x) => Math.log10(x)),
    guard(n = "exp",   fn (x) => Math.exp(x)),
    guard(n = "sqrt",  fn (x) => Math.sqrt(x)),
    guard(n = "sqr",   fn (x) => x * x)
  ]));

  val consts = flat_map(identifier, fn(n) => fold([
    guard(n = "E", 	      Math.e),
    guard(n = "PI",       Math.pi),
    guard(n = "LOG2E",    1.44269504088896340736),  (* log2(e) *)
    guard(n = "LOG10E",   0.434294481903251827651), (* log10(e) *)
    guard(n = "LN2",      0.693147180559945309417), (* ln(2) *)
    guard(n = "LN10", 	  2.30258509299404568402),  (* ln(10) *)
    guard(n = "PI_2", 	  1.57079632679489661923),  (* pi/2 *)
    guard(n = "PI_4", 	  0.785398163397448309616), (* pi/4 *)
    guard(n = "1_PI", 	  0.318309886183790671538), (* 1/pi *)
    guard(n = "2_PI", 	  0.636619772367581343076), (* 2/pi *)
    guard(n = "2_SQRTPI", 1.12837916709551257390),  (* 2/sqrt(pi) *)
    guard(n = "SQRT2",    1.41421356237309504880),  (* sqrt(2) *)
    guard(n = "SQRT1_2",  0.707106781186547524401)  (* 1/sqrt(2) *)
  ]));

  fun expr() = flat_map(usign, fn (sgn) => chainl1(term(), or_else(add, fn () => sub), sgn = "-"))
  and term() = chainl1(factor(), or_else(mul, fn () => div_), false)
  and factor() = chainr1(factor0(), pow)
  and factor0() = or_else(or_else(or_else(expr_in_brackets(),
      fn () => apply(funcs, expr_in_brackets)),
      fn () => consts),
      fn () => double)
  and expr_in_brackets() = between(symbol(#"("), symbol(#")"), expr);

  fun calculate(s: string) = expr()(s);

end