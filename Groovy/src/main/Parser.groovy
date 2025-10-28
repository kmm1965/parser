package main

class Parser<A> {
    Closure<Optional<Tuple2<A, String>>> unp

    Optional<Tuple2<A, String>> parse(String inp) {
        return unp(inp)
    }

    // Functor
    def <B> Parser<B> map(Closure<B> f){
         return flatMap { A a -> pure(f(a)) }
    }

    // Applicative
    static <A> Parser<A> pure(A a){
        return new Parser<A>(unp: { String inp -> Optional.of(new Tuple2(a, inp)) })
    }

    Parser apply(Closure fp){
        // q is invoked only if pf parsed successfully.
        return flatMap { Closure f -> fp().map(f) }
    }

    // Monad
    def <B> Parser<B> flatMap(Closure<Parser<B>> f){
        return new Parser<B>(unp: { String inp -> parse(inp).flatMap { pair -> f(pair.getV1()).parse(pair.getV2()) } })
    }

    def <B> Parser<B> skip(Closure<Parser<B>> fp){
        return flatMap { _ -> fp() }
    }

    // Alternative
    static <A> Parser<A> empty(){
        return new Parser<A>(unp: { String inp -> Optional.empty() })
    }

    Parser<A> orElse(Closure<Parser<A>> fp){
        return new Parser<A>(unp: { String inp -> Maybe.orElseGet(parse(inp), { fp().parse(inp) }) })
    }

    Parser<String> some(){
        return map { char c -> { String s -> c.toString() + s } }.apply { many() }
    }

    Parser<String> many(){
        return some().orElse { pure("") }
    }
}
