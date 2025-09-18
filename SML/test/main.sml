use "../src/calculator.sml";

open Calculator
open Maybe

fun maybeToString(m: (real * string) Maybe) =
  case m of
    Just p => "(" ^ Real.toString(#1 p) ^ "," ^ #2 p ^ ")"
  | Nothing => "Nothing";

print(maybeToString(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")) ^ "\n"); (* -8.889 *)
print(maybeToString(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")) ^ "\n");      (* -0.363408573143 *)
print(maybeToString(calculate("sqr(sin(2)) + sqr(cos(1 + 1))")) ^ "\n");         (* 1.0 *)
print(maybeToString(calculate("3 ^ 2 ^ 3")) ^ "\n");                             (* 6561.0 *)
print(maybeToString(calculate(" E ^ PI ")) ^ "\n");                              (* 23.1406926328 *)
print(maybeToString(calculate(" PI ^ E ")) ^ "\n");                              (* 22.4591577184 *)
