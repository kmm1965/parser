open System
open Calculator

[<EntryPoint>]
let main argv =
    let expr: Parser.Parser<float> = Calculator.Expr()
    printfn "%f" (fst (expr.Parse "72 - 7 - (1 - 2) * 3").Value)
    printfn "%f" (fst (expr.Parse "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").Value)
    printfn "%f" (fst (expr.Parse "sqr( sin (2)) + sqr(cos(1 + 1))").Value)
    printfn "%f" (fst (expr.Parse "3 ^ 2 ^ 3").Value)

    0 // return an integer exit code
