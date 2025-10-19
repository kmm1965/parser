open System
open Calculator

[<EntryPoint>]
let main argv =
    let expr: Parser.Parser<float> = Calculator.Expr()
    printfn "%s" (expr.Parse("72 - 7 - (1 - 2) * 3").ToString())
    printfn "%s" (expr.Parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").ToString()) // -8.889
    printfn "%s" (expr.Parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").ToString())      // -0.363408573142653
    printfn "%s" (expr.Parse("sqr( sin (2)) + sqr(cos(1 + 1))").ToString())       // 1.0
    printfn "%s" (expr.Parse("3 ^ 2 ^ 3").ToString())                             // 6561.0
    printfn "%s" (expr.Parse("sin(-PI/4)").ToString())                            // -0,707106781186547
    printfn "%s" (expr.Parse(" E ^ PI ").ToString())                              // 23.1406926327793
    printfn "%s" (expr.Parse(" PI ^ E ").ToString())                              // 22.4591577183610

    0 // return an integer exit code
