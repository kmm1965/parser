namespace MonadParser
{
    public class Parser<T>
    {
        private readonly Func<string, Maybe<(T, string)>> unp;

        public Parser(Func<string, Maybe<(T, string)>> unp)
        {
            this.unp = unp;
        }

        public Maybe<(T, string)> Parse(string inp) => unp(inp);

        // Functor
        public Parser<TResult> Map<TResult>(Func<T, TResult> mapping)
        {
            Parser<T> self = this;
            return new Parser<TResult>(inp => self.Parse(inp).Map(pair => (mapping(pair.Item1), pair.Item2)));
        }

        // Monad
        public Parser<TResult> FlatMap<TResult>(Func<T, Parser<TResult>> fp)
        {
            Parser<T> self = this;
            return new Parser<TResult>(inp => self.Parse(inp).FlatMap(pair => fp(pair.Item1).Parse(pair.Item2)));
        }

        public Parser<U> Skip<U>(Parser<U> p) => FlatMap(_ => p);

        public Parser<U> Skip<U>(Func<Parser<U>> fp) => FlatMap(_ => fp());

        // Applicative
        public static Parser<T> Pure(T value) => new Parser<T>(inp => Maybe.Some((value, inp)));

        public static Parser<TResult> Apply<TResult>(Parser<Func<T, TResult>> pf, Func<Parser<T>> fq) => pf.FlatMap(func => fq().Map(func));

        // Alternative
        public static Parser<T> Empty() => new Parser<T>(inp => Maybe<(T, string)>.Empty());

        public Parser<T> OrElse(Parser<T> p)
        {
            Parser<T> self = this;
            return new Parser<T>(inp => self.Parse(inp) | (() => p.Parse(inp)));
        }

        public static Parser<T> operator |(Parser<T> p, Parser<T> other) => p.OrElse(other);

        public static Parser<string> Optional(Parser<string> p) => p | Parser<string>.Pure("");

        public static Parser<string> Optional(Parser<char> p) => Optional(p.Map(c => c.ToString()));

        public Parser<string> Some() => Parser<string>.Apply(Map<Func<string, string>>(c => s => c + s), Many);

        public Parser<string> Many() => Optional(Some());

        public static readonly Parser<char> anyChar = new Parser<char>(inp =>
            inp.Length > 0 ?
                Maybe<(char, string)>.Pure((inp[0], inp.Substring(1))) :
                Maybe<(char, string)>.Empty());

        public static Parser<char> Satisfy(Func<char, bool> pred) => anyChar.FlatMap(c => pred(c) ? Parser<char>.Pure(c) : Parser<char>.Empty());

        public static Parser<char> _Char(char c) => Satisfy(c1 => c1 == c);

        public static Parser<char> alnum => Satisfy(c => Char.IsLetterOrDigit(c) || c == '_');

        public static Parser<string> spaces => Satisfy(Char.IsWhiteSpace).Many();

        public Parser<T> Token() => spaces.Skip(this).FlatMap(a => spaces.Skip(Parser<T>.Pure(a)));

        public static Parser<char> Symbol(char x) => Satisfy(c => c == x).Token();

        public static Parser<string> Name(string n) => alnum.Some()
            .FlatMap(s => s.Equals(n) ? Parser<string>.Pure(n) : Parser<string>.Empty()).Token();

        private static Parser<string> digits => Satisfy(Char.IsDigit).Many();

        private static Parser<string> sign => Optional(_Char('+') | _Char('-'));

        //public static Parser<double> _double => Satisfy(Char.IsDigit).Some().FlatMap(s => Parser<double>.Pure(double.Parse(s))).Token();
        public static Parser<double> _double => sign
            .FlatMap(sign_part => digits
            .FlatMap(int_part  => Optional(_Char('.').Skip(digits))
            .FlatMap(frac_part =>
                Optional((_Char('e') | _Char('E'))
                    .Skip(sign)
                    .FlatMap(exp_sign => Satisfy(Char.IsDigit).Some()
                    .FlatMap(exp_digits => Parser<string>.Pure(exp_sign + exp_digits)))
                )
            .FlatMap(exp_part  => int_part.Length > 0 || frac_part.Length > 0 ?
                Parser<double>.Pure(double.Parse(
                    sign_part + int_part +
                    (frac_part.Length > 0 ? ',' + frac_part : "") +
                    (exp_part.Length > 0 ? 'e' + exp_part : ""))) :
                Parser<double>.Empty())))).Token();

        private static Parser<T> Rest(Func<Parser<T>> fval, Func<T, Parser<T>> ff, Parser<Func<T, T, T>> op, T a) =>
            op.FlatMap(f => fval().FlatMap(b => ff(f(a, b)))) | Parser<T>.Pure(a);

        public Parser<T> Rest_l(Parser<Func<T, T, T>> op, T a)
        {
            Parser<T> self = this;
            return Rest(() => self, b => self.Rest_l(op, b), op, a);
        }

        public Parser<T> Rest_r(Parser<Func<T, T, T>> op, T a)
        {
            Parser<T> self = this;
            return Rest(() => self.Chainr1(op), Parser<T>.Pure, op, a);
        }

        public Parser<T> Chainl1(Parser<Func<T, T, T>> op) => FlatMap(a => Rest_l(op, a));

        public Parser<T> Chainr1(Parser<Func<T, T, T>> op) => FlatMap(a => Rest_r(op, a));

        public static Parser<T> Between<Open, Close>(Parser<Open> open, Parser<Close> close, Func<Parser<T>> fp) =>
            open.Skip(fp).FlatMap(e => close.Skip(Parser<T>.Pure(e)));
    }
}
