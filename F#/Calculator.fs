module Calculator

open System
open Parser
open SomeParsers

let Sqr x = x * x

type Calculator(unit) =
    static member Op2 (c: char) (f: float -> float -> float): Parser<float -> float -> float> =
        SomeParsers.Symbol(c).Skip(fun unit -> Parser.Pure f)

    static member add = Calculator.Op2 '+' (fun x y -> x + y)
    static member sub = Calculator.Op2 '-' (fun x y -> x - y)
    static member mul = Calculator.Op2 '*' (fun x y -> x * y)
    static member div = Calculator.Op2 '/' (fun x y -> x / y)
    static member pow = Calculator.Op2 '^' (fun x y -> Math.Pow(x, y))

    static member DefObject (name: string) (value: 'A): Parser<'A> = SomeParsers.Name(name).Skip(fun unit -> Parser.Pure value)

    //static member functions = [
    static member func: Parser<float -> float> = 
        (        Calculator.DefObject "sin"   Math.Sin)
         .OrElse(Calculator.DefObject "cos"   Math.Cos)
         .OrElse(Calculator.DefObject "asin"  Math.Asin)
         .OrElse(Calculator.DefObject "acos"  Math.Acos)
         .OrElse(Calculator.DefObject "sinh"  Math.Sinh)
         .OrElse(Calculator.DefObject "cosh"  Math.Cosh)
         .OrElse(Calculator.DefObject "tan"   Math.Tan)
         .OrElse(Calculator.DefObject "tanh"  Math.Tanh)
         .OrElse(Calculator.DefObject "log"   Math.Log)
         .OrElse(Calculator.DefObject "log10" Math.Log10)
         .OrElse(Calculator.DefObject "exp"   Math.Exp)
         .OrElse(Calculator.DefObject "sqrt"  Math.Sqrt)
         .OrElse(Calculator.DefObject "sqr"   Sqr)

    static member _const: Parser<float> = 
        (        Calculator.DefObject "E"        Math.E)
         .OrElse(Calculator.DefObject "PI"       Math.PI)
         .OrElse(Calculator.DefObject "LOG2E"    1.44269504088896340736)  // log2(e)
         .OrElse(Calculator.DefObject "LOG10E"   0.434294481903251827651) // log10(e)
         .OrElse(Calculator.DefObject "LN2"      0.693147180559945309417) // ln(2)
         .OrElse(Calculator.DefObject "LN10"     2.30258509299404568402)  // ln(10)
         .OrElse(Calculator.DefObject "PI_2"     1.57079632679489661923)  // pi/2
         .OrElse(Calculator.DefObject "PI_4"     0.785398163397448309616) // pi/4
         .OrElse(Calculator.DefObject "1_PI"     0.318309886183790671538) // 1/pi
         .OrElse(Calculator.DefObject "2_PI"     0.636619772367581343076) // 2/pi
         .OrElse(Calculator.DefObject "2_SQRTPI" 1.12837916709551257390)  // 2/sqrt(pi)
         .OrElse(Calculator.DefObject "SQRT2"    1.41421356237309504880)  // sqrt(2)
         .OrElse(Calculator.DefObject "SQRT1_2"  0.707106781186547524401) // 1/sqrt(2)

    static member ExprInBrackets unit: Parser<float> = Parser<float>.Between (Symbol '(') (Symbol ')') Calculator.Expr

    static member Factor0 unit: Parser<float> =
        Calculator.ExprInBrackets()
            .OrElse(Parser.Apply Calculator.func Calculator.ExprInBrackets)
            .OrElse(Calculator._const)
            .OrElse(double)

    static member Factor unit: Parser<float> = Calculator.Factor0().Chainr1 Calculator.pow

    static member Term unit: Parser<float> = Parser<float>.Chainl1(Calculator.Factor(), Calculator.mul.OrElse Calculator.div, false)

    static member Expr unit: Parser<float> = usign.FlatMap(fun sgn -> Parser<float>.Chainl1(Calculator.Term(), Calculator.add.OrElse Calculator.sub, sgn.Equals("-")))
