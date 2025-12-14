import java.util.Optional

class Parser<A>(val parse: (String) -> Optional<Pair<A, String>>) {
    fun <B> map(f: (A) -> B) : Parser<B> {
        return flatMap { a -> pure(f(a)) }
    }

    fun <B> flatMap(f: (A) -> Parser<B>) : Parser<B> {
        return Parser { inp -> this.parse(inp).flatMap { (a, out) -> f(a).parse(out) } }
    }

    fun <B> skip(fp: () -> Parser<B>): Parser<B> {
        return flatMap { _ -> fp() }
    }

    fun or(f: () -> Parser<A>) : Parser<A> {
        return Parser { inp -> this.parse(inp).or { f().parse(inp) } }
    }

    companion object {
        fun <A> pure(a: A) : Parser<A> {
            return Parser { inp -> Optional.of(Pair(a, inp)) }
        }

        fun <A> empty() : Parser<A> {
            return Parser { _ -> Optional.empty() }
        }

        fun <A, B> apply(pf: Parser<(A) -> B>, p: () -> Parser<A>): Parser<B> {
            return pf.flatMap { f -> p().map(f) }
        }
    }
}
