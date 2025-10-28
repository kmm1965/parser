package main

class SomeParsers {

    static Parser<Character> anyChar = new Parser<Character>(unp: { String inp ->
        !inp.isEmpty() ?
            Optional.of(new Tuple2(inp[0] as char, inp.substring(1))) :
            Optional.empty()
    })

    static Parser<Character> satisfy(Closure<Boolean> pred) {
        return anyChar.flatMap { char c -> pred(c) ? Parser.pure(c) : Parser.empty() }
    }

    static Parser<Character> char_(char c) {
        return satisfy { char c1 -> c1 == c }
    }

    static Parser<String> empty_string = Parser.pure("")

    static <O, C, A> Parser<A> between(Parser<O> open, Parser<C> close, Closure<Parser<A>> p) {
        return open.skip { p().flatMap { A a -> close.skip { Parser.pure(a) } } }
    }

    static Parser<String> spaces = satisfy { char c -> c.isWhitespace() }.many()

    static <A> Parser<A> token(Parser<A> p) {
        return between(spaces, spaces, { p })
    }

    static Parser<Character> symbol(char c) {
        return token(char_(c))
    }

    static Parser<Character> alnum = satisfy { char c -> c.isLetterOrDigit() || c == '_' as char }

    static Parser<String> name(String n) {
        return token(alnum.some().flatMap { String s -> s == n ? Parser.pure(n) : Parser.empty() })
    }

    static Parser<String> optional_s(Parser<String> p) {
        return p.orElse { empty_string }
    }

    static Parser<String> optional_c(Parser<Character> p) {
        return optional_s(p.map { char c -> c.toString() })
    }

    static Parser<Character> digit = satisfy { char c -> c.isDigit() }

    static Parser<String> digits = digit.many()

    static Parser<String> sign = optional_c(char_('+' as char).orElse { char_('-' as char) })

    // Unary sign
    static Parser<String> usign = token(sign)

    static Parser<Double> double_ = token(digits.flatMap {
        String int_part -> optional_s(char_('.' as char).skip { digits }).flatMap {
        String frac_part -> optional_s(char_('e' as char).orElse { char_('E' as char) }.skip { sign }.flatMap {
            String exp_sign -> digit.some().flatMap {
            String exp_digits -> Parser.pure(exp_sign + exp_digits) } }).flatMap {
        String exp_part ->
            if (!int_part.isEmpty() || !frac_part.isEmpty()) {
                Parser.pure((int_part +
                    (!frac_part.isEmpty() ? '.' + frac_part : "") +
                    (!exp_part.isEmpty() ? 'e' + exp_part : "")).toDouble())
            } else { Parser.empty() } } } })

    static Parser<Double> rest(Closure<Parser<Double>> fp, Closure<Double> ff, Parser<Closure<Double>> op, double a) {
        return op.flatMap { Closure<Double> f -> fp().flatMap { double b -> ff(f(a, b)) } }.orElse { Parser.pure(a) }
    }

    static Parser<Double> rest_l(Parser<Double> p, Parser<Closure<Double>> op, double a) {
        return rest({ p }, { double b -> rest_l(p, op, b) }, op, a)
    }

    static Parser<Double> rest_r(Parser<Double> p, Parser<Closure<Double>> op, double a) {
        return rest({ chainr1(p, op) }, { double b -> Parser.pure(b) }, op, a)
    }

    static Parser<Double> chainl1(Parser<Double> p, Parser<Closure<Double>> op, boolean negate_first) {
        return p.flatMap { double a -> rest_l(p, op, negate_first ? -a : a) }
    }

    static Parser<Double> chainr1(Parser<Double> p, Parser<Closure<Double>> op) {
        return p.flatMap { double a -> rest_r(p, op, a) }
    }
}
