module Calculator

open System
open Parser

let Sqr x = x * x

type Calculator(unit) =
    static member Op2 (c: char) (f: float -> float -> float): Parser<float -> float -> float> =
        Parser<char>.Symbol(c).Skip(fun unit -> Parser.Pure f)

    static member add = Calculator.Op2 '+' (fun x y -> x + y)
    static member sub = Calculator.Op2 '-' (fun x y -> x - y)
    static member mul = Calculator.Op2 '*' (fun x y -> x * y)
    static member div = Calculator.Op2 '/' (fun x y -> x / y)
    static member pow = Calculator.Op2 '^' (fun x y -> Math.Pow(x, y))

    static member Fold (parsers: List<Parser<'A>>): Parser<'A> =
        let mutable p0 = Parser<'A>.Empty()
        for p in parsers do
            p0 <- p0.OrElse(p)
        p0

    static member DefObject (name: string) (value: 'A): Parser<'A> = (Parser<string>.Name name).Skip(fun unit -> Parser.Pure(value))

    static member functions = [
        Calculator.DefObject "sin"   Math.Sin;
        Calculator.DefObject "cos"   Math.Cos;
        Calculator.DefObject "asin"  Math.Asin;
        Calculator.DefObject "acos"  Math.Acos;
        Calculator.DefObject "sinh"  Math.Sinh;
        Calculator.DefObject "cosh"  Math.Cosh;
        Calculator.DefObject "tan"   Math.Tan;
        Calculator.DefObject "tanh"  Math.Tanh;
        Calculator.DefObject "log"   Math.Log;
        Calculator.DefObject "log10" Math.Log10;
        Calculator.DefObject "exp"   Math.Exp;
        Calculator.DefObject "sqrt"  Math.Sqrt;
        Calculator.DefObject "sqr"   Sqr
    ]

    static member func: Parser<float -> float> = (Calculator.Fold Calculator.functions)

    static member M_LOG2E:float    = 1.44269504088896340736  // log2(e)
    static member M_LOG10E:float   = 0.434294481903251827651 // log10(e)
    static member M_LN2:float      = 0.693147180559945309417 // ln(2)
    static member M_LN10:float     = 2.30258509299404568402  // ln(10)
    static member M_PI_2:float     = 1.57079632679489661923  // pi/2
    static member M_PI_4:float     = 0.785398163397448309616 // pi/4
    static member M_1_PI:float     = 0.318309886183790671538 // 1/pi
    static member M_2_PI:float     = 0.636619772367581343076 // 2/pi
    static member M_2_SQRTPI:float = 1.12837916709551257390  // 2/sqrt(pi)
    static member M_SQRT2:float    = 1.41421356237309504880  // sqrt(2)
    static member M_SQRT1_2:float  = 0.707106781186547524401 // 1/sqrt(2)

    static member constants = [
        Calculator.DefObject "E"        Math.E;
        Calculator.DefObject "PI"       Math.PI;
        Calculator.DefObject "LOG2E"    Calculator.M_LOG2E;
        Calculator.DefObject "LOG10E"   Calculator.M_LOG10E;
        Calculator.DefObject "LN2"      Calculator.M_LN2;
        Calculator.DefObject "LN10"     Calculator.M_LN10;
        Calculator.DefObject "PI_2"     Calculator.M_PI_2;
        Calculator.DefObject "PI_4"     Calculator.M_PI_4;
        Calculator.DefObject "1_PI"     Calculator.M_1_PI;
        Calculator.DefObject "2_PI"     Calculator.M_2_PI;
        Calculator.DefObject "2_SQRTPI" Calculator.M_2_SQRTPI;
        Calculator.DefObject "SQRT2"    Calculator.M_SQRT2;
        Calculator.DefObject "SQRT1_2"  Calculator.M_SQRT1_2
    ]

    static member _const: Parser<float> = (Calculator.Fold Calculator.constants)

    static member br_open:  Parser<char> = Parser<char>.Symbol('(')
    static member br_close: Parser<char> = Parser<char>.Symbol(')')

    static member ExprInBrackets unit: Parser<float> = Parser<float>.Between Calculator.br_open Calculator.br_close Calculator.Expr

    static member Factor0 unit: Parser<float> =
        Calculator.ExprInBrackets()
            .OrElse(Parser.Apply Calculator.func Calculator.ExprInBrackets)
            .OrElse(Calculator._const)
            .OrElse(Parser<float>.natural)

    static member Factor unit: Parser<float> = Calculator.Factor0().Chainr1 Calculator.pow

    static member Term unit: Parser<float> = Calculator.Factor().Chainl1(Calculator.mul.OrElse Calculator.div)

    static member Expr unit: Parser<float> = Calculator.Term().Chainl1(Calculator.add.OrElse Calculator.sub)
