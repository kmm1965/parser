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

    // Alternative
    public static <A> Parser<A> empty(){
        return new Parser<>(inp -> Optional.empty());
    }

    public Parser<A> orElse(Parser<A> p){
        return new Parser<>(inp -> Alternative.orElseGet(parse(inp), () -> p.parse(inp)));
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

    public final static Parser<Character> digit = satisfy(Character::isDigit);

    public final static Parser<Character> alnum = satisfy(c -> Character.isLetterOrDigit(c) || c == '_');

    public static Parser<Character> symbol(char x){
        return satisfy(c -> c == x).token();
    }

    public final static Parser<String> spaces = satisfy(Character::isWhitespace).many();

    public Parser<A> token(){
        return spaces.flatMap(s -> flatMap(a -> spaces.flatMap(s2 -> Parser.pure(a))));
    }

    public final static Parser<Double> natural = digit.some().flatMap(s -> Parser.pure(Double.valueOf(s))).token();

    public static Parser<String> _name(String n){
        return alnum.some().flatMap(s -> s.equals(n) ? pure(s) : empty());
    }

    private static <A> Parser<A> rest(A a, Parser<BinaryOperator<A>> op, Supplier<Parser<A>> fval, Function<A, Parser<A>> ff){
        return op
            .flatMap(f -> fval.get().flatMap(b -> ff.apply(f.apply(a, b))))
            .orElse(Parser.pure(a));
    }

    public Parser<A> rest_l(A a, Parser<BinaryOperator<A>> op){
        return rest(a, op, () -> this, b -> rest_l(b, op));
    }

    public Parser<A> chainl1(Parser<BinaryOperator<A>> op){
        return flatMap(a -> rest_l(a, op));
    }

    public Parser<A> scan(Parser<BinaryOperator<A>> op){
        return flatMap(a -> rest_r(a, op));
    }

    public Parser<A> rest_r(A a, Parser<BinaryOperator<A>> op){
        return rest(a, op, () -> scan(op), Parser::pure);
    }

    public Parser<A> chainr1(Parser<BinaryOperator<A>> op){
        return scan(op);
    }

    public static <Open, Close, A> Parser<A> between(Parser<Open> open, Parser<Close> close, Supplier<Parser<A>> fp){
        return open.flatMap(c1 -> fp.get())
            .flatMap(e -> close.flatMap(c2 -> Parser.pure(e)));
    }
}
