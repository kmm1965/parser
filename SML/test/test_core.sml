structure Testing =
struct

fun cmp_gen(x: ''a * string, y: ''a * string): bool = #1 x = #1 y andalso #2 x = #2 y

fun cmp_real(x: real * string, y: real * string): bool = Real.abs(#1 x - #1 y) < 1e~13 andalso #2 x = #2 y

fun test(m1: 'a Maybe.Maybe, m2: 'a Maybe.Maybe, cmpeq: 'a * 'a -> bool): bool =
  case m1 of
     Maybe.Just x => (case m2 of
        Maybe.Just y => cmpeq(x, y)
      | Maybe.Nothing => false)
   | Maybe.Nothing => (case m2 of
        Maybe.Just _ => false
      | Maybe.Nothing => true);

fun ptestg(m1: (''a * string) Maybe.Maybe, m2: (''a * string) Maybe.Maybe) = print(Bool.toString(test(m1, m2, cmp_gen)) ^ "\n");

fun ptestr(m1: (real * string) Maybe.Maybe, m2: (real * string) Maybe.Maybe) = print(Bool.toString(test(m1, m2, cmp_real)) ^ "\n");

end