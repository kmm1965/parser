module Calculator

open System
open Parser
open SomeParsers

type Calculator(unit) =
    static member Op2 (c: char) (f: float -> float -> float): Parser<float -> float -> float> =
        SomeParsers.Symbol(c).Skip(fun unit -> Parser.Pure f)

    static member add = Calculator.Op2 '+' (fun x y -> x + y)
    static member sub = Calculator.Op2 '-' (fun x y -> x - y)
    static member mul = Calculator.Op2 '*' (fun x y -> x * y)
    static member div = Calculator.Op2 '/' (fun x y -> x / y)
    static member pow = Calculator.Op2 '^' (fun x y -> Math.Pow(x, y))

    static member Guard(b: bool, value: 'A): Parser<'A> = if b then Parser.Pure value else Parser<'A>.Empty()

    static member funcs: Parser<float -> float> = SomeParsers.identifier.FlatMap(fun n ->
        (        Calculator.Guard((n.Equals "sin"),   Math.Sin))
         .OrElse(Calculator.Guard((n.Equals "cos"),   Math.Cos))
         .OrElse(Calculator.Guard((n.Equals "asin"),  Math.Asin))
         .OrElse(Calculator.Guard((n.Equals "acos"),  Math.Acos))
         .OrElse(Calculator.Guard((n.Equals "sinh"),  Math.Sinh))
         .OrElse(Calculator.Guard((n.Equals "cosh"),  Math.Cosh))
         .OrElse(Calculator.Guard((n.Equals "tan"),   Math.Tan))
         .OrElse(Calculator.Guard((n.Equals "tanh"),  Math.Tanh))
         .OrElse(Calculator.Guard((n.Equals "log"),   Math.Log))
         .OrElse(Calculator.Guard((n.Equals "log10"), Math.Log10))
         .OrElse(Calculator.Guard((n.Equals "exp"),   Math.Exp))
         .OrElse(Calculator.Guard((n.Equals "sqrt"),  Math.Sqrt))
         .OrElse(Calculator.Guard((n.Equals "sqr"),   fun x -> x * x)))

    static member consts: Parser<float> = SomeParsers.identifier.FlatMap(fun n ->
        (        Calculator.Guard((n.Equals "E"),        Math.E))
         .OrElse(Calculator.Guard((n.Equals "PI"),       Math.PI))
         .OrElse(Calculator.Guard((n.Equals "LOG2E"),    1.44269504088896340736))   // log2(e)
         .OrElse(Calculator.Guard((n.Equals "LOG10E"),   0.434294481903251827651))  // log10(e)
         .OrElse(Calculator.Guard((n.Equals "LN2"),      0.693147180559945309417))  // ln(2)
         .OrElse(Calculator.Guard((n.Equals "LN10"),     2.30258509299404568402))   // ln(10)
         .OrElse(Calculator.Guard((n.Equals "PI_2"),     1.57079632679489661923))   // pi/2
         .OrElse(Calculator.Guard((n.Equals "PI_4"),     0.785398163397448309616))  // pi/4
         .OrElse(Calculator.Guard((n.Equals "1_PI"),     0.318309886183790671538))  // 1/pi
         .OrElse(Calculator.Guard((n.Equals "2_PI"),     0.636619772367581343076))  // 2/pi
         .OrElse(Calculator.Guard((n.Equals "2_SQRTPI"), 1.12837916709551257390))   // 2/sqrt(pi)
         .OrElse(Calculator.Guard((n.Equals "SQRT2"),    1.41421356237309504880))   // sqrt(2)
         .OrElse(Calculator.Guard((n.Equals "SQRT1_2"),  0.707106781186547524401))) // 1/sqrt(2)

    static member ExprInBrackets unit: Parser<float> = Parser<float>.Between (Symbol '(') (Symbol ')') Calculator.Expr

    static member Factor0 unit: Parser<float> =
        Calculator.ExprInBrackets()
            .OrElse(Parser.Apply Calculator.funcs Calculator.ExprInBrackets)
            .OrElse(Calculator.consts)
            .OrElse(double)

    static member Factor unit: Parser<float> = Calculator.Factor0().Chainr1 Calculator.pow

    static member Term unit: Parser<float> = Parser<float>.Chainl1(Calculator.Factor(), Calculator.mul.OrElse Calculator.div, false)

    static member Expr unit: Parser<float> = usign.FlatMap(fun sgn -> Parser<float>.Chainl1(Calculator.Term(), Calculator.add.OrElse Calculator.sub, sgn.Equals("-")))
