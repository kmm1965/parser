use "../src/maybe.sml";

open Maybe

fun cmp_gen(x: ''a, y: ''a): bool = x = y

fun cmp_real(x: real, y: real): bool = Real.abs(x - y) < 1e~12

fun test(m1: 'a Maybe, m2: 'a Maybe, cmpeq: 'a * 'a -> bool): bool =
  case m1 of
     Just x => (case m2 of
        Just y => cmpeq(x, y)
      | Nothing => false)
   | Nothing => (case m2 of
        Just _ => false
      | Nothing => true);

fun ptestg(m1: ''a Maybe, m2: ''a Maybe) = print(Bool.toString(test(m1, m2, cmp_gen)) ^ "\n");

fun ptestr(m1: real Maybe, m2: real Maybe) = print(Bool.toString(test(m1, m2, cmp_real)) ^ "\n");

fun test_maybe_map() =
  ptestr(map(Just 1.0, Math.sin), Just(Math.sin(1.0)));
  ptestr(map(Nothing, Math.sin), Nothing);
  ptestg(map(Just 1, Int.toString), Just "1");
  ptestg(map(Nothing, Int.toString), Nothing);

fun safe_sqrt(x: real): real Maybe = if x >= 0.0 then Just(Math.sqrt(x)) else Nothing;

fun safe_log(x: real): real Maybe = if x > 0.0 then Just(Math.ln(x)) else Nothing;

fun toString(i: int): string Maybe = if i mod 2 = 0 then Just(Int.toString(i)) else Nothing

fun test_maybe_flat_map() =
  ptestr(safe_sqrt(2.0), Just(Math.sqrt(2.0)));
  ptestr(safe_sqrt(0.0), Just 0.0);
  ptestr(safe_sqrt(~2.0), Nothing);

  ptestr(safe_log(2.0), Just(Math.ln(2.0)));
  ptestr(safe_log(0.0), Nothing);
  ptestr(safe_log(~2.0), Nothing);

  ptestr(flat_map(Just 2.0, safe_sqrt), Just(Math.sqrt(2.0)));
  ptestr(flat_map(Just 0.0, safe_sqrt), Just 0.0);
  ptestr(flat_map(Just ~2.0, safe_sqrt), Nothing);

  ptestr(flat_map(flat_map(Just 2.0, safe_sqrt), safe_log), Just(Math.ln(Math.sqrt(2.0))));
  ptestr(flat_map(flat_map(Just 0.0, safe_sqrt), safe_log), Nothing);
  ptestr(flat_map(flat_map(Just ~2.0, safe_sqrt), safe_log), Nothing);

  ptestg(flat_map(Just 2, toString), Just "2");
  ptestg(flat_map(Just 1, toString), Nothing);
  ptestg(flat_map(Nothing, toString), Nothing);

fun test_maybe_or_else() =
  ptestr(or_else(Just 1.0, fn () => Just 2.0), Just 1.0);
  ptestr(or_else(Nothing, fn () => Just 2.0), Just 2.0);

test_maybe_map();
test_maybe_flat_map();
test_maybe_or_else();