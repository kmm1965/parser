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

  fun defObject(n: string, value: 'a) =
    skip(name(n), fn () => pure(value));

  val funcs = fold([
    defObject("sin", fn (x) => Math.sin(x)),
    defObject("cos", fn (x) => Math.cos(x)),
    defObject("asin", fn (x) => Math.asin(x)),
    defObject("acos", fn (x) => Math.acos(x)),
    defObject("sinh", fn (x) => Math.sinh(x)),
    defObject("cosh", fn (x) => Math.cosh(x)),
    defObject("tan", fn (x) => Math.tan(x)),
    defObject("log", fn (x) => Math.ln(x)),
    defObject("log10", fn (x) => Math.log10(x)),
    defObject("exp", fn (x) => Math.exp(x)),
    defObject("sqrt", fn (x) => Math.sqrt(x)),
    defObject("sqr", fn (x) => x * x)
  ]);

  val consts = fold([
    defObject("E", 	      Math.e),             (* e *)
    defObject("PI",       Math.pi),            (* pi *)
    defObject("LOG2E",    1.4426950408889634), (* log2(e) *)
    defObject("LOG10E",   0.4342944819032518), (* log10(e) *)
    defObject("LN2",      0.6931471805599453), (* ln(2) *)
    defObject("LN10", 	  2.302585092994046),  (* ln(10) *)
    defObject("PI_2", 	  1.5707963267948966), (* pi/2 *)
    defObject("PI_4", 	  0.7853981633974483), (* pi/4 *)
    defObject("1_PI", 	  0.3183098861837907), (* 1/pi *)
    defObject("2_PI", 	  0.6366197723675814), (* 2/pi *)
    defObject("2_SQRTPI", 1.1283791670955126), (* 2/sqrt(pi) *)
    defObject("SQRT2",    1.4142135623730951), (* sqrt(2) *)
    defObject("SQRT1_2",  0.7071067811865476)  (* 1/sqrt(2) *)
  ]);

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