using MonadParser;

Parser<double> expr = Calculator.Expr();

Console.WriteLine(expr.Parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").Value.Item1);
Console.WriteLine(expr.Parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").Value.Item1);
Console.WriteLine(expr.Parse("sqr(sin(2)) + sqr(cos(1 + 1))").Value.Item1);
Console.WriteLine(expr.Parse("3 ^ 2 ^ 3").Value.Item1);
