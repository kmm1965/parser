namespace MonadParser
{
    class SomeParsers
    {
        public static Parser<char> alnum => Parser<char>.Satisfy(c => Char.IsLetterOrDigit(c) || c == '_');

        public static Parser<char> _Char(char c) => Parser<char>.Satisfy(x => x == c);

        public static Parser<char> Symbol(char c) => _Char(c).Token();

        public static Parser<string> Name(string n) => alnum.Some()
            .FlatMap(s => s.Equals(n) ? Parser<string>.Pure(n) : Parser<string>.Empty()).Token();

        private static Parser<string> digits => Parser<char>.Satisfy(Char.IsDigit).Many();

        private static Parser<string> sign => Parser<char>.Optional(_Char('+') | _Char('-'));

        private static string NumberDecimalSeparator = System.Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator;

        public static Parser<double> _double => sign.FlatMap(
            sign_part => digits.FlatMap(
            int_part  => Parser<char>.Optional(_Char('.').Skip(digits)).FlatMap(
            frac_part => Parser<char>.Optional((_Char('e') | _Char('E')).Skip(sign).FlatMap(
                exp_sign   => Parser<char>.Satisfy(Char.IsDigit).Some().FlatMap(
                exp_digits => Parser<string>.Pure(exp_sign + exp_digits)))).FlatMap(
            exp_part => int_part.Length > 0 || frac_part.Length > 0 ?
                Parser<double>.Pure(double.Parse(
                    sign_part + int_part +
                    (frac_part.Length > 0 ? NumberDecimalSeparator + frac_part : "") +
                    (exp_part.Length > 0 ? 'e' + exp_part : ""))) :
                Parser<double>.Empty())))).Token();

    }
}
