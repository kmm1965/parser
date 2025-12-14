import java.util.Optional

class SomeParsers {
    companion object {
        val anyChar: Parser<Char> = Parser { inp ->
            if(!inp.isEmpty()){
                Optional.of(Pair(inp[0], inp.substring(1)))
            } else { Optional.empty() }
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
            return Parser.apply(p.map{ c: Char -> { s: String -> c + s } }) { many(p) }
        }

        fun many(p: Parser<Char>): Parser<String>{
            return some(p).or { empty_string }
        }

        val spaces: Parser<String> = many(satisfy { c -> c.isWhitespace() })

        fun <A> token(p: Parser<A>): Parser<A>{
            return between(spaces, spaces) { p }
        }

        fun symbol(c: Char): Parser<Char>{
            return token(char(c))
        }

        val alnum: Parser<Char> = satisfy { c -> c.isLetterOrDigit() || c == '_' }

        val identifier: Parser<String> = token(some(alnum))

        fun name(n:String): Parser<String>{
            return identifier.flatMap { s -> if(s == n){ Parser.pure(n)} else { Parser.empty() } }
        }

        fun optionalS(p: Parser<String>) : Parser<String> {
            return p.or { empty_string }
        }

        fun optionalC(p: Parser<Char>) : Parser<String> {
            return optionalS(p.map { c -> c.toString() })
        }

        val digit: Parser<Char> = satisfy { c -> c.isDigit() }

        val digits: Parser<String> = many(digit)

        val sign: Parser<String> = optionalC(char('+').or { char('-') })

        // Unary sign
        val usign: Parser<String> = token(sign)

        val double: Parser<Double> = token(digits.flatMap {
            intPart -> optionalS(char('.').skip { digits }).flatMap {
            fracPart -> optionalS(char('e').or { char('E') }.skip { sign }.flatMap {
                expSign -> some(digit).flatMap {
                expDigits -> Parser.pure(expSign + expDigits) } }).flatMap {
            expPart -> if(intPart.isNotEmpty() || fracPart.isNotEmpty()){
                Parser.pure((intPart +
                    (if(fracPart.isNotEmpty()){ ".$fracPart" } else { "" }) +
                    (if(expPart.isNotEmpty()){ "e$expPart" } else { "" })).toDouble())
                } else { Parser.empty() } } } })

        fun <A> rest(p: () -> Parser<A>, ff: (A) -> Parser<A>, op: Parser<(A, A) -> A>, a: A): Parser<A>{
            return op.flatMap { f -> p().flatMap { b -> ff(f(a, b)) } }.or { Parser.pure(a) }
        }

        fun <A> restL(p: Parser<A>, op: Parser<(A, A) -> A>, a: A): Parser<A>{
            return rest({ p }, { b -> restL(p, op,  b) }, op, a)
        }

        fun <A> restR(p: Parser<A>, op: Parser<(A, A) -> A>, a: A): Parser<A>{
            return rest({ chainr1(p, op) }, { b -> Parser.pure(b) }, op, a)
        }

        fun chainl1(p: Parser<Double>, op: Parser<(Double, Double) -> Double>, negateFirst: Boolean): Parser<Double>{
            return p.flatMap { a -> restL(p, op, if(negateFirst){ -a } else { a }) }
        }

        fun <A> chainr1(p: Parser<A>, op: Parser<(A, A) -> A>): Parser<A>{
            return p.flatMap { a -> restR(p, op, a) }
        }
    }
}
