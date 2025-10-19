using MonadParser;

Parser<double> expr = Calculator.Expr();

Console.WriteLine(expr.Parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").Value.Item1); // -8.889
Console.WriteLine(expr.Parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").Value.Item1);      // -0.3634085731426532
Console.WriteLine(expr.Parse("sqr(sin(2)) + sqr(cos(1 + 1))").Value.Item1);         // 1
Console.WriteLine(expr.Parse("3 ^ 2 ^ 3").Value.Item1);                             // 6561
Console.WriteLine(expr.Parse("sin(-PI/4)").Value.Item1);                            // -0,7071067811865476
Console.WriteLine(expr.Parse(" E ^ PI ").Value.Item1);                              // 23.140692632779263
Console.WriteLine(expr.Parse(" PI ^ E ").Value.Item1);                              // 22.45915771836104
