module SomeParsers

open System // Char
open Parser

let alnum: Parser<char> = Parser<char>.Satisfy(fun c -> Char.IsLetterOrDigit(c) || c.Equals '_')

let Name (n: string): Parser<string> = alnum.Some().FlatMap(fun s -> if s.Equals(n) then Parser.Pure(n) else Parser<string>.Empty()).Token()

let _Char(c: char): Parser<char> = Parser<char>.Satisfy(_.Equals(c))

let Symbol(c: char): Parser<char> = _Char(c).Token()

let digits: Parser<string> = (Parser<char>.Satisfy Char.IsDigit).Many()

let sign: Parser<string> = Parser<char>.Optional_c(_Char('+').OrElse(_Char '-'))

let NumberDecimalSeparator = System.Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator

let double: Parser<float> = sign.FlatMap(
    fun sign_part -> digits.FlatMap(
    fun int_part  -> Parser<string>.Optional_s(_Char('.').Skip(digits)).FlatMap(
    fun frac_part -> Parser<string>.Optional_s(_Char('e').OrElse(_Char 'E').Skip(sign).FlatMap(
        fun exp_sign   -> Parser<string>.Satisfy(Char.IsDigit).Some().FlatMap(
        fun exp_digits -> Parser<string>.Pure(exp_sign + exp_digits)))).FlatMap(
    fun exp_part ->
        if int_part.Length > 0 || frac_part.Length > 0
        then Parser<float>.Pure(Double.Parse(sign_part + int_part +
            (if frac_part.Length > 0 then NumberDecimalSeparator + frac_part else "") +
            (if exp_part.Length > 0 then "e" + exp_part else "")))
        else Parser<float>.Empty())))).Token()
