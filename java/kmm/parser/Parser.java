package kmm.parser;

import javafx.util.Pair;
import kmm.utils.Alternative;

import java.util.Optional;
import java.util.function.*;

public class Parser<A> {
    private final Function<String, Optional<Pair<A, String>>> p;

    public Parser(Function<String, Optional<Pair<A, String>>> p){
        this.p = p;
    }

    public Optional<Pair<A, String>> parse(String inp){
        return p.apply(inp);
    }

    // Functor
    public <B> Parser<B> map(Function<? super A, B> f){
        return new Parser<>(inp -> parse(inp).map(pair -> new Pair<>(f.apply(pair.getKey()), pair.getValue())));
    }

    // Applicative
    public static <A> Parser<A> pure(A a){
        return new Parser<>(inp -> Optional.of(new Pair<>(a, inp)));
    }

    public static <A, B> Parser<B> apply(Parser<Function<A, B>> pf, Supplier<Parser<A>> q){
        // q is invoked only if pf parsed successfully.
        return pf.flatMap(f -> q.get().map(f));
    }

    public static <T> Parser<T> apply_u(Parser<UnaryOperator<T>> pf, Supplier<Parser<T>> q){
        // q is invoked only if pf parsed successfully.
        return pf.flatMap(f -> q.get().map(f));
    }

    // Monad
    public <B> Parser<B> flatMap(Function<? super A, Parser<B>> f){
        return new Parser<>(inp -> parse(inp).flatMap(pair -> f.apply(pair.getKey()).parse(pair.getValue())));
    }

    public <B> Parser<B> skip(Parser<B> p){
        return flatMap(a -> p);
    }

    public <B> Parser<B> skip(Supplier<Parser<B>> fp){
        return flatMap(a -> fp.get());
    }

    // Alternative
    public static <A> Parser<A> empty(){
        return new Parser<>(inp -> Optional.empty());
    }

    public Parser<A> orElse(Parser<A> p){
        return new Parser<>(inp -> Alternative.orElseGet(parse(inp), () -> p.parse(inp)));
    }

    public Parser<A> orElseGet(Supplier<Parser<A>> fp){
        return new Parser<>(inp -> Alternative.orElseGet(parse(inp), () -> fp.get().parse(inp)));
    }

    public Parser<String> some(){
        return apply(map(c -> s -> c + s), this::many);
    }

    public Parser<String> many(){
        return some().orElse(pure(""));
    }

    public final static Parser<Character> anyChar = new Parser<>(inp -> !inp.isEmpty() ?
        Optional.of(new Pair<>(inp.charAt(0), inp.substring(1))) :
        Optional.empty());

    public static Parser<Character> satisfy(Predicate<Character> f){
        return anyChar.flatMap(c -> f.test(c) ? Parser.pure(c) : Parser.empty());
    }

    public final static Parser<Character> alnum = satisfy(c -> Character.isLetterOrDigit(c) || c == '_');

    public static Parser<Character> _char(char x){
        return satisfy(c -> c == x);
    }

    public final static Parser<String> spaces = satisfy(Character::isWhitespace).many();

    public Parser<A> token(){
        return spaces.skip(this).flatMap(a -> spaces.skip(Parser.pure(a)));
    }

    public static Parser<Character> symbol(char x){
        return satisfy(c -> c == x).token();
    }

    public static Parser<String> name(String n){
        return alnum.some().flatMap(s -> s.equals(n) ? pure(s) : empty()).token();
    }

    public static Parser<String> optional_s(Parser<String> p){
        return p.orElse(Parser.pure(""));
    }

    public static Parser<String> optional_c(Parser<Character> p){
        return optional_s(p.map(Object::toString));
    }

    public final static Parser<String> digits = satisfy(Character::isDigit).many();

    public final static Parser<String> sign = optional_c(_char('+').orElse(_char('-')));

    public final static Parser<Double> _double = (
        sign.flatMap(sign_part ->
        digits.flatMap(int_part ->
        optional_s(_char('.').skip(digits))
            .flatMap(frac_part ->
        optional_s(
            _char('e').orElse(_char('E')).skip(sign)
                .flatMap(exp_sign ->
            satisfy(Character::isDigit).some()
                .flatMap(exp_digits ->
                    pure(exp_sign + exp_digits)))
        ).flatMap(exp_part ->
            !int_part.isEmpty() || !frac_part.isEmpty() ?
                pure(Double.valueOf(sign_part + int_part +
                    (!frac_part.isEmpty() ? '.' + frac_part : "") +
                    (!exp_part.isEmpty() ? 'e' + exp_part : ""))) :
                empty()))))
    ).token();

    public final static Parser<Double> _double_ = satisfy(Character::isDigit).some()
        .flatMap(s -> Parser.pure(Double.valueOf(s))).token();

    private static <A> Parser<A> rest(Supplier<Parser<A>> fval, Function<A, Parser<A>> ff, Parser<BinaryOperator<A>> op, A a){
        return op
            .flatMap(f -> fval.get().flatMap(b -> ff.apply(f.apply(a, b))))
            .orElse(Parser.pure(a));
    }

    public Parser<A> rest_l(Parser<BinaryOperator<A>> op, A a){
        return rest(() -> this, b -> rest_l(op, b), op, a);
    }

    public Parser<A> rest_r(Parser<BinaryOperator<A>> op, A a){
        return rest(() ->  chainr1(op), Parser::pure, op, a);
    }

    public Parser<A> chainl1(Parser<BinaryOperator<A>> op){
        return flatMap(a -> rest_l(op, a));
    }

    public Parser<A> chainr1(Parser<BinaryOperator<A>> op){
        return flatMap(a -> rest_r(op, a));
    }

    public static <Open, Close, A> Parser<A> between(Parser<Open> open, Parser<Close>close, Supplier<Parser<A>> fp){
        return open.skip(fp).flatMap(x -> close.skip(Parser.pure(x)));
    }
}
