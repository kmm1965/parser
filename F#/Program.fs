open System
open Calculator

[<EntryPoint>]
let main argv =
    let expr: Parser.Parser<float> = Calculator.Expr()
    printfn "%s" (expr.Parse("72 - 7 - (1 - 2) * 3").ToString())
    printfn "%s" (expr.Parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").ToString())
    printfn "%s" (expr.Parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").ToString())
    printfn "%s" (expr.Parse("sqr( sin (2)) + sqr(cos(1 + 1))").ToString())
    printfn "%s" (expr.Parse("3 ^ 2 ^ 3").ToString())

    0 // return an integer exit code
