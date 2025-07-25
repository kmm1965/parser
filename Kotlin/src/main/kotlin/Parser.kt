import arrow.core.Option
import arrow.core.Some
import arrow.core.none
import arrow.core.orElse

class Parser<A>(val parse: (String) -> Option<Pair<A, String>>) {
    fun <B> map(f: (A) -> B) : Parser<B> {
        return Parser { inp -> this.parse(inp).map { (a, out) -> Pair(f(a), out) } }
    }

    fun <B> flatMap(f: (A) -> Parser<B>) : Parser<B> {
        return Parser { inp -> this.parse(inp).flatMap { (a, out) -> f(a).parse(out) } }
    }

    fun <B> skip(fp: () -> Parser<B>): Parser<B> {
        return flatMap { _ -> fp() }
    }

    fun orElse(f: () -> Parser<A>) : Parser<A> {
        return Parser { inp -> this.parse(inp).orElse { f().parse(inp) } }
    }

    companion object {
        fun <A> pure(a: A) : Parser<A> {
            return Parser { inp -> Some(Pair(a, inp)) }
        }

        fun <A> empty() : Parser<A> {
            return Parser { _ -> none() }
        }

        fun <A, B> apply(pf: Parser<(A) -> B>, p: () -> Parser<A>): Parser<B> {
            return pf.flatMap { f -> p().map(f) }
        }
    }
}
