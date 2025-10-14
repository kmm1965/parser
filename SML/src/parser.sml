use "../src/maybe.sml";

structure Parser =
struct
  type 'a Parser = string -> ('a * string) Maybe.Maybe

  fun pure(x: 'a): 'a Parser =
    fn(inp) => Maybe.Just((x, inp))

  val 'a empty: 'a Parser = fn(inp) => Maybe.Nothing

  fun flat_map(p: 'a Parser, f: 'a -> 'b Parser): 'b Parser =
    fn (inp) => Maybe.flat_map(p(inp), fn (p) => f(#1 p)(#2 p));

  fun map(p: 'a Parser, f: 'a -> 'b): 'b Parser =
    flat_map(p, fn (x) => pure(f x))

  fun skip(p: 'a Parser, f: unit -> 'b Parser): 'b Parser =
    flat_map(p, fn (_) => f());

  fun apply(pf: ('a -> 'b) Parser, fp: unit -> 'a Parser): 'b Parser =
    flat_map(pf, fn (f) => map(fp(), f));

  fun or_else(p: 'a Parser, fp: unit -> 'a Parser): 'a Parser =
    fn (inp) => Maybe.or_else(p(inp), fn () => fp()(inp));

end