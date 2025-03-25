namespace MonadParser
{
    public class Calculator
    {
        private static Parser<Func<double, double, double>> Op2(char c, Func<double, double, double> f) =>
            SomeParsers.Symbol(c).Skip(() => Parser<Func<double, double, double>>.Pure(f));

        private static readonly Parser<Func<double, double, double>> add = Op2('+', (x, y) => x + y);
        private static readonly Parser<Func<double, double, double>> sub = Op2('-', (x, y) => x - y);
        private static readonly Parser<Func<double, double, double>> mul = Op2('*', (x, y) => x * y);
        private static readonly Parser<Func<double, double, double>> div = Op2('/', (x, y) => x / y);
        private static readonly Parser<Func<double, double, double>> pow = Op2('^', Math.Pow);

        private static Parser<A> DefObject<A>(string name, A value) => SomeParsers.Name(name).Skip(() => Parser<A>.Pure(value));
        private static Parser<Func<double, double>> DefFunc(string name, Func<double, double> func) => DefObject(name, func);

        private static double Sqr(double x) => x * x;

        private static readonly Parser<Func<double, double>> func =
            DefFunc("sin",   Math.Sin)   |
            DefFunc("cos",   Math.Cos)   |
            DefFunc("asin",  Math.Asin)  |
            DefFunc("acos",  Math.Acos)  |
            DefFunc("sinh",  Math.Sinh)  |
            DefFunc("cosh",  Math.Cosh)  |
            DefFunc("tan",   Math.Tan)   |
            DefFunc("log",   Math.Log)   |
            DefFunc("log2",  Math.Log2)  |
            DefFunc("log10", Math.Log10) |
            DefFunc("exp",   Math.Exp)   |
            DefFunc("sqrt",  Math.Sqrt)  |
            DefFunc("sqr",   Sqr);

        private static readonly double M_LOG2E = 1.44269504088896340736;    // log2(e)
        private static readonly double M_LOG10E = 0.434294481903251827651;  // log10(e)
        private static readonly double M_LN2 = 0.693147180559945309417;     // ln(2)
        private static readonly double M_LN10 = 2.30258509299404568402;     // ln(10)
        private static readonly double M_PI_2 = 1.57079632679489661923;     // pi/2
        private static readonly double M_PI_4 = 0.785398163397448309616;    // pi/4
        private static readonly double M_1_PI = 0.318309886183790671538;    // 1/pi
        private static readonly double M_2_PI = 0.636619772367581343076;    // 2/pi
        private static readonly double M_2_SQRTPI = 1.12837916709551257390; // 2/sqrt(pi)
        private static readonly double M_SQRT2 = 1.41421356237309504880;    // sqrt(2)
        private static readonly double M_SQRT1_2 = 0.707106781186547524401; // 1/sqrt(2)

        private static readonly Parser<Double> _const =
            DefObject("E",        Math.E)     |
            DefObject("PI",       Math.PI)    |
            DefObject("LOG2E",    M_LOG2E)    |
            DefObject("LOG10E",   M_LOG10E)   |
            DefObject("LN2",      M_LN2)      |
            DefObject("LN10",     M_LN10)     |
            DefObject("PI_2",     M_PI_2)     |
            DefObject("PI_4",     M_PI_4)     |
            DefObject("1_PI",     M_1_PI)     |
            DefObject("2_PI",     M_2_PI)     |
            DefObject("2_SQRTPI", M_2_SQRTPI) |
            DefObject("SQRT2",    M_SQRT2)    |
            DefObject("SQRT1_2",  M_SQRT1_2);

        private static readonly Parser<char> br_open = SomeParsers.Symbol('(');
        private static readonly Parser<char> br_close = SomeParsers.Symbol(')');

        private static Parser<double> ExprInBrackets(){
            return Parser<double>.Between(br_open, br_close, Expr);
        }

        private static Parser<double> Factor0(){
            return ExprInBrackets()
                | Parser<double>.Apply(func, ExprInBrackets)
                | _const
                | SomeParsers._double;
        }

        private static Parser<double> Factor() => Factor0().Chainr1(pow);

        private static Parser<double> Term() => Factor().Chainl1(mul | div);

        public static Parser<double> Expr() => Term().Chainl1(add | sub);
    }
}
