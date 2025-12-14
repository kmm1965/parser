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

        private static Parser<A> Guard<A>(bool b, A value) => b ? Parser<A>.Pure(value) : Parser<A>.Empty();

        private static Parser<Func<double, double>> GuardFn(bool b, Func<double, double> fn) => Guard(b, fn);

        private static readonly Parser<Func<double, double>> funcs =
            SomeParsers.Indentifier.FlatMap(n =>
                GuardFn(n.Equals("sin"),   Math.Sin)   |
                GuardFn(n.Equals("cos"),   Math.Cos)   |
                GuardFn(n.Equals("asin"),  Math.Asin)  |
                GuardFn(n.Equals("acos"),  Math.Acos)  |
                GuardFn(n.Equals("sinh"),  Math.Sinh)  |
                GuardFn(n.Equals("cosh"),  Math.Cosh)  |
                GuardFn(n.Equals("tan"),   Math.Tan)   |
                GuardFn(n.Equals("log"),   Math.Log)   |
                GuardFn(n.Equals("log2"),  Math.Log2)  |
                GuardFn(n.Equals("log10"), Math.Log10) |
                GuardFn(n.Equals("exp"),   Math.Exp)   |
                GuardFn(n.Equals("sqrt"),  Math.Sqrt)  |
                GuardFn(n.Equals("sqr"),   x => x * x));

        private static readonly Parser<Double> consts =
            SomeParsers.Indentifier.FlatMap(n =>
                Guard(n.Equals("E"),        Math.E)  |
                Guard(n.Equals("PI"),       Math.PI) |
                Guard(n.Equals("LOG2E"),    1.44269504088896340736)  | // log2(e)
                Guard(n.Equals("LOG10E"),   0.434294481903251827651) | // log10(e)
                Guard(n.Equals("LN2"),      0.693147180559945309417) | // ln(2)
                Guard(n.Equals("LN10"),     2.30258509299404568402)  | // ln(10)
                Guard(n.Equals("PI_2"),     1.57079632679489661923)  | // pi/2
                Guard(n.Equals("PI_4"),     0.785398163397448309616) | // pi/4
                Guard(n.Equals("1_PI"),     0.318309886183790671538) | // 1/pi
                Guard(n.Equals("2_PI"),     0.636619772367581343076) | // 2/pi
                Guard(n.Equals("2_SQRTPI"), 1.12837916709551257390)  | // 2/sqrt(pi)
                Guard(n.Equals("SQRT2"),    1.41421356237309504880)  | // sqrt(2)
                Guard(n.Equals("SQRT1_2"),  0.707106781186547524401)); // 1/sqrt(2)

        private static Parser<double> ExprInBrackets(){
            return Parser<double>.Between(SomeParsers.Symbol('('), SomeParsers.Symbol(')'), Expr);
        }

        private static Parser<double> Factor0(){
            return ExprInBrackets()
                | Parser<double>.Apply(funcs, ExprInBrackets)
                | consts
                | SomeParsers.double_;
        }

        private static Parser<double> Factor() => Factor0().Chainr1(pow);

        private static Parser<double> Term() => Parser<double>.Chainl1(Factor(), mul | div);

        public static Parser<double> Expr() => SomeParsers.usign.FlatMap(sgn => Parser<double>.Chainl1(Term(), add | sub, sgn == "-"));
    }
}
