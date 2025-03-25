package kmm.parser;

public class SomeParsers {
    public final static Parser<Character> alnum = Parser.satisfy(c -> Character.isLetterOrDigit(c) || c == '_');

    public static Parser<Character> _char(char c){
        return Parser.satisfy(x -> x == c);
    }

    public static Parser<Character> symbol(char c){
        return _char(c).token();
    }

    public static Parser<String> name(String n){
        return alnum.some().flatMap(s -> s.equals(n) ? Parser.pure(n) : Parser.empty()).token();
    }

    public static Parser<String> optional_s(Parser<String> p){
        return p.orElse(Parser.pure(""));
    }

    public static Parser<String> optional_c(Parser<Character> p){
        return optional_s(p.map(Object::toString));
    }

    public final static Parser<String> digits = Parser.satisfy(Character::isDigit).many();

    public final static Parser<String> sign = optional_c(_char('+').orElse(_char('-')));

    public final static Parser<Double> _double = sign.flatMap(
        sign_part -> digits.flatMap(
        int_part  -> optional_s(_char('.').skip(digits)).flatMap(
        frac_part -> optional_s(_char('e').orElse(_char('E')).skip(sign).flatMap(
            exp_sign -> Parser.satisfy(Character::isDigit).some().flatMap(
            exp_digits -> Parser.pure(exp_sign + exp_digits)))).flatMap(
        exp_part -> !int_part.isEmpty() || !frac_part.isEmpty() ?
            Parser.pure(Double.valueOf(sign_part + int_part +
                (!frac_part.isEmpty() ? '.' + frac_part : "") +
                (!exp_part.isEmpty() ? 'e' + exp_part : ""))) :
            Parser.empty())))).token();

    public final static Parser<Double> _double_ = Parser.satisfy(Character::isDigit).some()
        .flatMap(s -> Parser.pure(Double.valueOf(s))).token();
}
