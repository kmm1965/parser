open Parser
open Some_parsers

let op2 c f = symbol c >>> pure f

let add = op2 '+' (fun x y -> x +. y)
let sub = op2 '-' (fun x y -> x -. y)
let mul = op2 '*' (fun x y -> x *. y)
let div = op2 '/' (fun x y -> x /. y)
let pow = op2 '^' (fun x y -> exp(y *. log(x)))

let guard (b : bool) (x : 'a) : 'a Parser.parser = if b then pure x else empty

let rec fold f acc lst = match lst with
    | [] -> acc
    | h :: t -> fold f (f acc h) t

let funcs = identifier >>= (fun n -> fold (<||>) empty [
    guard (String.equal n "sin")   sin;
    guard (String.equal n "cos")   cos;
    guard (String.equal n "asin")  asin;
    guard (String.equal n "acos")  acos;
    guard (String.equal n "sinh")  sinh;
    guard (String.equal n "cosh")  cosh;
    guard (String.equal n "asinh") asinh;
    guard (String.equal n "acosh") acosh;
    guard (String.equal n "tan")   tan;
    guard (String.equal n "atan")  atan;
    guard (String.equal n "log")   log;
    guard (String.equal n "log10") log10;
    guard (String.equal n "exp")   exp;
    guard (String.equal n "sqrt")  sqrt;
    guard (String.equal n "sqr")   (fun x -> x *. x)
])

let m_PI       = 3.14159265358979323846   (* pi *)

let consts = identifier >>= (fun n -> fold (<||>) empty [
    guard (String.equal n "E")        2.71828182845904523536;
    guard (String.equal n "PI")       3.14159265358979323846;
    guard (String.equal n "LOG2E")    1.44269504088896340736;  (* log2(e) *)
    guard (String.equal n "LOG10E")   0.434294481903251827651; (* log10(e) *)
    guard (String.equal n "LN2")      0.693147180559945309417; (* ln(2) *)
    guard (String.equal n "LN10")     2.30258509299404568402;  (* ln(10) *)
    guard (String.equal n "PI_2")     1.57079632679489661923;  (* pi/2 *)
    guard (String.equal n "PI_4")     0.785398163397448309616; (* pi/4 *)
    guard (String.equal n "1_PI")     0.318309886183790671538; (* 1/pi *)
    guard (String.equal n "2_PI")     0.636619772367581343076; (* 2/pi *)
    guard (String.equal n "2_SQRTPI") 1.12837916709551257390;  (* 2/sqrt(pi) *)
    guard (String.equal n "SQRT2")    1.41421356237309504880;  (* sqrt(2) *)
    guard (String.equal n "SQRT1_2")  0.707106781186547524401  (* 1/sqrt(2) *)
])

let rec expr = function
    () -> usign >>= (fun sgn -> chainl1 (term ()) (add <||> sub) (String.equal sgn "-"))
and term = function
    () -> chainl1 (factor ()) (mul <||> div) false
and factor = function
    () -> chainr1 (factor0 ()) pow
and factor0 = function
    () -> expr_in_brackets () <||>
        (funcs <*> expr_in_brackets) <||>
        consts <||> double
and expr_in_brackets = function
    () -> between_get (symbol '(') (symbol ')') expr

let calculate s = parse (expr ()) s
