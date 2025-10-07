namespace MonadParser
{
    class SomeParsers
    {
        public static Parser<char> alnum => Parser<char>.Satisfy(c => Char.IsLetterOrDigit(c) || c == '_');

        public static Parser<char> Char_(char c) => Parser<char>.Satisfy(x => x == c);

        public static Parser<char> Symbol(char c) => Char_(c).Token();

        public static Parser<string> Name(string n) => alnum.Some()
            .FlatMap(s => s.Equals(n) ? Parser<string>.Pure(n) : Parser<string>.Empty()).Token();

        public static Parser<char> digit => Parser<char>.Satisfy(Char.IsDigit);

        public static Parser<string> digits => digit.Many();

        public static Parser<string> sign => Parser<char>.Optional(Char_('+') | Char_('-'));
        public static Parser<string> usign => Parser<char>.Optional(Symbol('+') | Symbol('-'));

        private static string NumberDecimalSeparator = System.Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator;

        public static Parser<double> double_ => digits.FlatMap(
            int_part  => Parser<char>.Optional(Char_('.').Skip(digits)).FlatMap(
            frac_part => Parser<char>.Optional((Char_('e') | Char_('E')).Skip(sign).FlatMap(
                exp_sign   => digit.Some().FlatMap(
                exp_digits => Parser<string>.Pure(exp_sign + exp_digits)))).FlatMap(
            exp_part => int_part.Length > 0 || frac_part.Length > 0 ?
                Parser<double>.Pure(double.Parse(int_part +
                    (frac_part.Length > 0 ? NumberDecimalSeparator + frac_part : "") +
                    (exp_part.Length > 0 ? 'e' + exp_part : ""))) :
                Parser<double>.Empty()))).Token();

    }
}
