use "../src/parser.sml";

open Parser

structure SomeParsers =
struct

  val empty_string = pure("");

  fun some(p: char Parser): string Parser =
    apply(map(p, fn (c) => fn(s: string) => String.str c ^ s), fn () => many(p))
  and many(p: char Parser): string Parser = or_else(some(p), fn () => empty_string);

  val anyChar: char Parser = fn (inp) =>
    if String.size inp > 0
    then Maybe.Just((String.sub(inp, 0), String.extract(inp, 1, NONE)))
    else Maybe.Nothing;

  fun satisfy(pred: char -> bool): char Parser =
    flat_map(anyChar, fn (c) => if pred c then pure c else empty);

  fun char(c: char): char Parser = satisfy(fn (x) => x = c);
  
  fun between(open_: 'o Parser, close_: 'c Parser, fp: unit -> 'a Parser): 'a Parser =
    skip(open_, fn () => flat_map(fp(), fn (x) => skip(close_, fn () => pure(x))));

  val spaces = many(satisfy(Char.isSpace));

  fun token(p: 'a Parser): 'a Parser = between(spaces, spaces, fn () => p);

  fun symbol(c: char): char Parser = token(char c);

  val alnum = satisfy(fn (c) => Char.isAlphaNum c orelse c = #"_");

  fun name(n: string) = token(flat_map(some(alnum), fn (n2) => if n2 = n then pure(n) else empty));

  fun optional_s(p: string Parser): string Parser = or_else(p, fn () => empty_string);

  fun optional_c(p: char Parser): string Parser = optional_s(map(p, fn (c) => String.str c));

  val digit = satisfy(Char.isDigit);

  val digits = many(digit);

  val sign = optional_c(or_else(char(#"+"), fn () => char(#"-")));

  (* Unary sign *)
  val usign = token(sign);
  
  val double = token(flat_map(digits,
    fn (int_part) => flat_map(optional_s(skip(char(#"."), fn () => digits)),
    fn (frac_part) => flat_map(optional_s(flat_map(skip(or_else(char(#"e"), fn () => char(#"E")), fn () => sign),
      fn (exp_sign) => flat_map(some(digit),
      fn (exp_digits) => pure(exp_sign ^ exp_digits)))),
    fn (exp_part) =>
      if String.size int_part > 0 orelse String.size frac_part > 0
      then pure(valOf(Real.fromString(int_part ^
              (if String.size frac_part > 0 then "." ^ frac_part else "") ^
              (if String.size exp_part > 0 then "e" ^ exp_part else ""))))
          else empty))));

  fun rest(fp: unit -> 'a Parser, ff: 'a -> 'a Parser, op_: ('a * 'a -> 'a) Parser, x: 'a): 'a Parser =
    or_else(flat_map(op_, fn (f) => flat_map(fp(), fn (y) => ff(f(x, y)))), fn () => pure(x));

  fun rest_l(p: 'a Parser, op_: ('a * 'a -> 'a) Parser, x: 'a): 'a Parser =
    rest(fn () => p, fn (y) => rest_l(p, op_, y), op_, x);

  fun chainl1(p: real Parser, op_: (real * real -> real) Parser, negate_first: bool): real Parser =
    flat_map(p, fn (x) => rest_l(p, op_, if negate_first then ~x else x));

  fun rest_r(p: 'a Parser, op_: ('a * 'a -> 'a) Parser, x: 'a): 'a Parser =
    rest(fn () => chainr1(p, op_), pure, op_, x)
  and chainr1(p: 'a Parser, op_: ('a * 'a -> 'a) Parser): 'a Parser =
    flat_map(p, fn (x) => rest_r(p, op_, x));

end