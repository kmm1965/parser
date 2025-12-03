package kmm.parser;

import kmm.utils.Pair;

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
        return new Parser<>(inp -> parse(inp).map(
            pair -> Pair.of(f.apply(pair.getFirst()), pair.getSecond())));
        //return flatMap(a -> pure(f.apply(a)));
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
        return new Parser<>(inp -> parse(inp).flatMap(
            pair -> f.apply(pair.getFirst()).parse(pair.getSecond())));
    }

    public <B> Parser<B> skip(Supplier<Parser<B>> fp){
        return flatMap(_ -> fp.get());
    }

    public <B> Parser<B> skip_p(Parser<B> p){
        return skip(() -> p);
    }

    // Alternative
    public static <A> Parser<A> empty(){
        return new Parser<>(_ -> Optional.empty());
    }

    public Parser<A> or(Supplier<Parser<A>> fp){
        return new Parser<>(inp -> parse(inp).or(() -> fp.get().parse(inp)));
    }

    public Parser<A> orElse(Parser<A> p){
        return or(() -> p);
    }

    public static final Parser<String> emptyString = Parser.pure("");

    public Parser<String> some(){
        return apply(map(c -> s -> c + s), this::many);
    }

    public Parser<String> many(){
        return some().orElse(emptyString);
    }

    public final static Parser<Character> anyChar = new Parser<>(inp -> !inp.isEmpty() ?
        Optional.of(new Pair<>(inp.charAt(0), inp.substring(1))) :
        Optional.empty());

    public static Parser<Character> satisfy(Predicate<Character> f){
        return anyChar.flatMap(c -> f.test(c) ? pure(c) : empty());
    }

    public final static Parser<String> spaces = satisfy(Character::isWhitespace).many();

    public static <Open, Close, A> Parser<A> between(Parser<Open> open, Parser<Close>close, Parser<A> p){
        return open.skip_p(p).flatMap(x -> close.skip(() -> pure(x)));
    }

    public static <Open, Close, A> Parser<A> between(Parser<Open> open, Parser<Close>close, Supplier<Parser<A>> fp){
        return open.skip(fp).flatMap(x -> close.skip(() -> pure(x)));
    }

    public Parser<A> token(){
        return between(spaces, spaces, this);
    }

    private static <A> Parser<A> rest(Supplier<Parser<A>> fval, Function<A, Parser<A>> ff, Parser<BinaryOperator<A>> op, A a){
        return op
            .flatMap(f -> fval.get().flatMap(b -> ff.apply(f.apply(a, b))))
            .or(() -> pure(a));
    }

    private Parser<A> rest_l(Parser<BinaryOperator<A>> op, A a){
        return rest(() -> this, b -> rest_l(op, b), op, a);
    }

    private Parser<A> rest_r(Parser<BinaryOperator<A>> op, A a){
        return rest(() ->  chainr1(op), Parser::pure, op, a);
    }

    public static Parser<Double> chainl1(Parser<Double> self, Parser<BinaryOperator<Double>> op, boolean negate_first){
        return self.flatMap(a -> self.rest_l(op, negate_first ? -a : a));
    }

    public static Parser<Double> chainl1(Parser<Double> self, Parser<BinaryOperator<Double>> op){
        return chainl1(self, op, false);
    }

    public Parser<A> chainr1(Parser<BinaryOperator<A>> op){
        return flatMap(a -> rest_r(op, a));
    }
}
