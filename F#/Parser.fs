open System

// Maybe (Option) support
// Functor
let inline (</>) (f: 'A -> 'B) (x: Option<'A>) = if x.IsSome then Some(f x.Value) else None

// Applicative
let inline (<*>) (f: Option<'A -> 'B>) (x: Option<'A>) = if f.IsSome then f.Value </> x else None

// Monad
let inline (>>=) (x: Option<'A>) (f: 'A -> Option<'B>) = if x.IsSome then f x.Value else None

// Alternative
let inline (<|>) (x: Option<'A>) (fy: unit -> Option<'A>) = if x.IsSome then x else fy()

type Parser<'A>(unp: string -> Option<'A * string>) =
    member this.unp = unp

    member this.Parse (inp: string): Option<'A * string> = this.unp(inp)

    // Functor
    member this.Map (f: 'A -> 'B) = Parser(fun inp -> this.Parse inp |> (</>) (fun (pair: 'A * string) -> (f(fst pair), snd pair)))

    // Monad
    member this.FlatMap (f: 'A -> Parser<'B>): Parser<'B> = Parser(fun inp -> this.Parse inp >>= (fun (pair: 'A * string) -> f(fst pair).Parse(snd pair)))

    member this.Skip (p: Parser<'B>): Parser<'B> = this.FlatMap(fun unit -> p)

    member this.Skip (fp: unit -> Parser<'B>): Parser<'B> = this.FlatMap(fun unit -> fp())

    // Applicative
    static member Pure (x: 'A) = Parser(fun inp -> Some((x, inp)))

    static member Apply (pf: Parser<'A -> 'B>) (q: unit -> Parser<'A>): Parser<'B> = pf.FlatMap(fun f -> q().Map f)

    // Alternative
    static member Empty unit = Parser(fun inp -> None)

    member this.OrElse (p: Parser<'A>): Parser<'A> = Parser(fun inp -> this.Parse inp <|> fun unit -> p.Parse inp)

    member this.Some unit: Parser<string> =
        ((fun c -> (fun (s: string) -> c.ToString() + s)) |> this.Map |> Parser.Apply) this.Many 

    member this.Many unit: Parser<string> = (this.Some()).OrElse(Parser.Pure "")

    static member anyChar: Parser<char> = Parser(fun inp -> if inp.Length > 0 then Some (inp[0], inp.Substring 1) else None)

    static member Satisfy(pred: char -> bool): Parser<char> = Parser<char>.anyChar.FlatMap(fun (c: char) -> if pred c then Parser.Pure c else Parser<char>.Empty())

    static member alnum: Parser<char> = Parser<char>.Satisfy(fun c -> Char.IsLetterOrDigit(c) || c.Equals '_')

    static member spaces: Parser<string> = Parser<char>.Satisfy(Char.IsWhiteSpace).Many()

    member this.Token unit: Parser<'A> = Parser<string>.spaces.Skip(fun unit -> this).FlatMap(fun a -> Parser<string>.spaces.Skip(fun unit -> Parser.Pure a))

    static member Symbol(x: char): Parser<char> = Parser<char>.Satisfy(_.Equals(x)).Token()

    static member Name (n: string): Parser<string> = Parser<char>.alnum.Some().FlatMap(fun s -> if s.Equals(n) then Parser.Pure(n).Token() else Parser<string>.Empty())

    static member natural: Parser<float> = (Parser<char>.Satisfy Char.IsDigit).Some().FlatMap(fun (s: string) -> Parser.Pure(Double.Parse s).Token())

    static member Rest (fval: unit -> Parser<'A>) (ff: 'A -> Parser<'A>) (op: Parser<'A -> 'A -> 'A>) (a: 'A): Parser<'A> =
        op.FlatMap(fun f -> fval().FlatMap(fun b -> ff(f a b)))
          .OrElse(Parser.Pure a)

    member this.Rest_l (op: Parser<'A -> 'A -> 'A>) (a: 'A): Parser<'A> = Parser.Rest (fun unit -> this) (this.Rest_l op) op a

    member this.Rest_r (op: Parser<'A -> 'A -> 'A>) = Parser.Rest (fun unit -> this.Chainr1 op) Parser.Pure op

    member this.Chainl1 (op: Parser<'A -> 'A -> 'A>): Parser<'A> = this.FlatMap (this.Rest_l op)

    member this.Chainr1 (op: Parser<'A -> 'A -> 'A>): Parser<'A> = this.FlatMap (this.Rest_r op)

    static member Between (_open: Parser<'Open>) (_close: Parser<'Close>) (fp: unit -> Parser<'A>): Parser<'A> =
        _open.Skip(fp).FlatMap(fun a -> _close.Skip(Parser.Pure(a)))

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

[<EntryPoint>]
let main argv =
    let expr: Parser<float> = Calculator.Expr()
    printfn "%f" (fst (expr.Parse "72 - 7 - (1 - 2) * 3").Value)
    printfn "%f" (fst (expr.Parse "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").Value)
    printfn "%f" (fst (expr.Parse "sqr( sin (2)) + sqr(cos(1 + 1))").Value)
    printfn "%f" (fst (expr.Parse "3 ^ 2 ^ 3").Value)

    0 // return an integer exit code
