open Parser
open Some_parsers

let def_object (n : string) (x : 'a) : 'a Parser.parser = name n >> (fun () ->  pure x)

let rec fold f acc lst = match lst with
    | [] -> acc
    | h :: t -> fold f (f acc h) t

let funcs = fold (<||>) empty [
    def_object "sin"   sin;
    def_object "cos"   cos;
    def_object "asin"  asin;
    def_object "acos"  acos;
    def_object "sinh"  sinh;
    def_object "cosh"  cosh;
    def_object "asinh" asinh;
    def_object "acosh" acosh;
    def_object "tan"   tan;
    def_object "atan"  atan;
    def_object "log"   log;
    def_object "exp"   exp;
    def_object "sqrt"  sqrt;
    def_object "sqr"   (fun x -> x *. x)]

let m_E        = 2.71828182845904523536   (* e *)
let m_LOG2E    = 1.44269504088896340736   (* log2(e) *)
let m_LOG10E   = 0.434294481903251827651  (* log10(e) *)
let m_LN2      = 0.693147180559945309417  (* ln(2) *)
let m_LN10     = 2.30258509299404568402   (* ln(10) *)
let m_PI       = 3.14159265358979323846   (* pi *)
let m_PI_2     = 1.57079632679489661923   (* pi/2 *)
let m_PI_4     = 0.785398163397448309616  (* pi/4 *)
let m_1_PI     = 0.318309886183790671538  (* 1/pi *)
let m_2_PI     = 0.636619772367581343076  (* 2/pi *)
let m_2_SQRTPI = 1.12837916709551257390   (* 2/sqrt(pi) *)
let m_SQRT2    = 1.41421356237309504880   (* sqrt(2) *)
let m_SQRT1_2  = 0.707106781186547524401  (* 1/sqrt(2) *)

let consts = fold (<||>) empty [
    def_object "E" m_E;
    def_object "LOG2E"    m_LOG2E;
    def_object "LOG10E"   m_LOG10E;
    def_object "LN2"      m_LN2;
    def_object "LN10"     m_LN10;
    def_object "PI"       m_PI;
    def_object "PI_2"     m_PI_2;
    def_object "PI_4"     m_PI_4;
    def_object "1_PI"     m_1_PI;
    def_object "2_PI"     m_2_PI;
    def_object "2_SQRTPI" m_2_SQRTPI;
    def_object "SQRT2"    m_SQRT2;
    def_object "SQRT1_2"  m_SQRT1_2]

let op2 c f = symbol c >>> pure f

let add = op2 '+' (fun x y -> x +. y)
let sub = op2 '-' (fun x y -> x -. y)
let mul = op2 '*' (fun x y -> x *. y)
let div = op2 '/' (fun x y -> x /. y)
let pow = op2 '^' (fun x y -> exp(y *. log(x)))

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
