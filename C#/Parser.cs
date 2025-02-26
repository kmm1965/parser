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

        // Applicative
        public static Parser<T> Pure(T value) => new Parser<T>(inp => Maybe.Some((value, inp)));

        public static Parser<TResult> Apply<TResult>(Parser<Func<T, TResult>> pf, Func<Parser<T>> fq) => pf.FlatMap(func => fq().Map(func));

        // Monad
        public Parser<TResult> FlatMap<TResult>(Func<T, Parser<TResult>> fp)
        {
            Parser<T> self = this;
            return new Parser<TResult>(inp => self.Parse(inp).FlatMap(pair => fp(pair.Item1).Parse(pair.Item2)));
        }

        // Alternative
        public static Parser<T> Empty() => new Parser<T>(inp => Maybe<(T, string)>.Empty());

        public Parser<T> OrElse(Parser<T> p)
        {
            Parser<T> self = this;
            return new Parser<T>(inp => self.Parse(inp) | (() => p.Parse(inp)));
        }

        public static Parser<T> operator |(Parser<T> p, Parser<T> other) => p.OrElse(other);

        public Parser<T> OrElseGet(Func<Parser<T>> fp)
        {
            Parser<T> self = this;
            return new Parser<T>(inp => self.Parse(inp) | (() => fp().Parse(inp)));
        }

        public static Parser<T> operator |(Parser<T> p, Func<Parser<T>> fp) => p.OrElseGet(fp);

        static public Parser<string> Some(Parser<char> p) => Parser<string>.Apply(p.Map<Func<string, string>>(c => s => c + s), () => Many(p));

        public static Parser<string> Many(Parser<char> p) => Some(p) | Parser<string>.Pure("");

        public static readonly Parser<char> anyChar = new Parser<char>(inp =>
            inp.Length > 0 ?
                Maybe<(char, string)>.Pure((inp[0], inp.Substring(1))) :
                Maybe<(char, string)>.Empty());

        public static Parser<char> Satisfy(Func<char, bool> pred) => anyChar.FlatMap(c => pred(c) ? Parser<char>.Pure(c) : Parser<char>.Empty());

        public static Parser<char> Digit => Satisfy(Char.IsDigit);

        public static Parser<char> Alnum => Satisfy(c => Char.IsLetterOrDigit(c) || c == '_');

        public static Parser<char> Symbol(char x) => Satisfy(c => c == x).Token();

        public static Parser<string> Spaces => Many(Satisfy(Char.IsWhiteSpace));

        public Parser<U> Skip<U>(Parser<U> p) => FlatMap(_ => p);

        public Parser<U> Skip<U>(Func<Parser<U>> fp) => FlatMap(_ => fp());

        public Parser<T> Token() => Spaces.Skip(this).FlatMap(a => Spaces.Skip(Parser<T>.Pure(a)));

        public static Parser<double> Natural => Some(Digit).FlatMap(s => Parser<double>.Pure(double.Parse(s)));

        public static Parser<string> Name(string n) => Some(Alnum).FlatMap(s => s.Equals(n) ? Parser<string>.Pure(n) : Parser<string>.Empty());

        public static Parser<T> Rest(T a, Parser<Func<T, T, T>> op, Func<Parser<T>> fval, Func<T, Parser<T>> ff) =>
            op.FlatMap(f => fval().FlatMap(b => ff(f(a, b)))) | Parser<T>.Pure(a);

        public Parser<T> Rest_l(T a, Parser<Func<T, T, T>> op)
        {
            Parser<T> self = this;
            return Rest(a, op, () => self, b => self.Rest_l(b, op));
        }

        public Parser<T> Chainl1(Parser<Func<T, T, T>> op) => FlatMap(a => Rest_l(a, op));

        public Parser<T> Scan(Parser<Func<T, T, T>> op) => FlatMap(a => Rest_r(a, op));

        public Parser<T> Rest_r(T a, Parser<Func<T, T, T>> op)
        {
            Parser<T> self = this;
            return Rest(a, op, () => self.Scan(op), Parser<T>.Pure);
        }

        public Parser<T> Chainr1(Parser<Func<T, T, T>> op) => Scan(op);


        public static Parser<T> Between<Open, Close>(Parser<Open> open, Parser<Close> close, Func<Parser<T>> fp) =>
            open.Skip(fp).FlatMap(e => close.Skip(Parser<T>.Pure(e)));
    }
}
