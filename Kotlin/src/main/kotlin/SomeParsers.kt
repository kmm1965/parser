import arrow.core.Some
import arrow.core.none

class SomeParsers {
    companion object {
        val anyChar: Parser<Char> = Parser { inp ->
            if(!inp.isEmpty()){
                Some(Pair(inp[0], inp.substring(1)))
            } else { none() }
        }

        fun satisfy(pred: (Char) -> Boolean): Parser<Char>{
            return anyChar.flatMap { c -> if(pred(c)){ Parser.pure(c) } else { Parser.empty() } }
        }

        fun char(c: Char) : Parser<Char>{
            return satisfy { c1 -> c1 == c }
        }

        val empty_string: Parser<String> = Parser.pure("")

        fun <A, O, C> between(open: Parser<O>, close: Parser<C>, p: () -> Parser<A>): Parser<A>{
            return open.skip { p().flatMap { x -> close.skip { Parser.pure(x) } } }
        }

        fun some(p: Parser<Char>): Parser<String>{
            return Parser.apply(p.map{ c: Char -> { s: String -> c + s } }, { many(p) })
        }

        fun many(p: Parser<Char>): Parser<String>{
            return some(p).orElse { empty_string }
        }

        val spaces: Parser<String> = many(satisfy { c -> c.isWhitespace() })

        fun <A> token(p: Parser<A>): Parser<A>{
            return between(spaces, spaces, { p })
        }

        fun symbol(c: Char): Parser<Char>{
            return token(char(c))
        }

        val alnum: Parser<Char> = satisfy { c -> c.isLetterOrDigit() || c == '_' }

        fun name(n:String): Parser<String>{
            return token(some(alnum).flatMap { s -> if(s.equals(n)){ Parser.pure(n)} else { Parser.empty() } })
        }

        fun optional_s(p: Parser<String>) : Parser<String> {
            return p.orElse { empty_string }
        }

        fun optional_c(p: Parser<Char>) : Parser<String> {
            return optional_s(p.map { c -> c.toString() })
        }

        val digit: Parser<Char> = satisfy { c -> c.isDigit() }

        val digits: Parser<String> = many(digit)

        val sign: Parser<String> = optional_c(char('+').orElse { char('-') })

        // Unary sign
        val usign: Parser<String> = token(sign)

        val double: Parser<Double> = token(digits.flatMap {
            int_part -> optional_s(char('.').skip { digits }).flatMap {
            frac_part -> optional_s(char('e').orElse { char('E') }.skip { sign }.flatMap {
                exp_sign -> some(digit).flatMap {
                exp_digits -> Parser.pure(exp_sign + exp_digits) } }).flatMap {
            exp_part -> if(int_part.isNotEmpty() || frac_part.isNotEmpty()){
                Parser.pure((int_part +
                    (if(frac_part.isNotEmpty()){ '.' + frac_part } else { "" }) +
                    (if(exp_part.isNotEmpty()){ 'e' + exp_part } else { "" })).toDouble())
                } else { Parser.empty() } } } })

        fun <A> rest(p: () -> Parser<A>, ff: (A) -> Parser<A>, op: Parser<(A, A) -> A>, a: A): Parser<A>{
            return op.flatMap { f -> p().flatMap { b -> ff(f(a, b)) } }.orElse { Parser.pure(a) }
        }

        fun <A> rest_l(p: Parser<A>, op: Parser<(A, A) -> A>, a: A): Parser<A>{
            return rest({ p }, { b -> rest_l(p, op,  b) }, op, a)
        }

        fun <A> rest_r(p: Parser<A>, op: Parser<(A, A) -> A>, a: A): Parser<A>{
            return rest({ chainr1(p, op) }, { b -> Parser.pure(b) }, op, a)
        }

        fun chainl1(p: Parser<Double>, op: Parser<(Double, Double) -> Double>, negate_first: Boolean): Parser<Double>{
            return p.flatMap { a -> rest_l(p, op, if(negate_first){ -a } else { a }) }
        }

        fun <A> chainr1(p: Parser<A>, op: Parser<(A, A) -> A>): Parser<A>{
            return p.flatMap { a -> rest_r(p, op, a) }
        }
    }
}
