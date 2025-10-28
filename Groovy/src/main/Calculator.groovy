package main

class Calculator {
    static Parser op2(char c, Closure f){
        return  SomeParsers.symbol(c).skip{ Parser.pure(f) }
    }
    static Parser add = op2('+' as char, { double x, double y -> x + y })
    static Parser sub = op2('-' as char, { double x, double y -> x - y })
    static Parser mul = op2('*' as char, { double x, double y -> x * y })
    static Parser div = op2('/' as char, { double x, double y -> x / y })
    static Parser pow = op2('^' as char, Math::pow)

    static double sqr(double x){
        return x * x
    }

    static Parser fold(List<Parser> parsers) {
        return parsers.inject(Parser.empty(), { Parser p0, Parser p -> p0.orElse { p } })
    }

    static  Parser def_object(String name, value){
        return SomeParsers.name(name).skip(() -> Parser.pure(value))
    }

    static Parser funcs = fold([
        def_object("sin",   Math::sin),
        def_object("cos",   Math::cos),
        def_object("asin",  Math::asin),
        def_object("acos",  Math::acos),
        def_object("sinh",  Math::sinh),
        def_object("cosh",  Math::cosh),
        def_object("tan",   Math::tan),
        def_object("log",   Math::log),
        def_object("log10", Math::log10),
        def_object("exp",   Math::exp),
        def_object("sqrt",  Math::sqrt),
        def_object("sqr",   Calculator::sqr)
    ])

    static Parser consts = fold([
        def_object("E",        Math.E),
        def_object("PI",       Math.PI),
        def_object("LOG2E",    1.44269504088896340736),  // log2(e)
        def_object("LOG10E",   0.434294481903251827651), // log10(e)
        def_object("LN2",      0.693147180559945309417), // ln(2)
        def_object("LN10",     2.30258509299404568402),  // ln(10)
        def_object("PI_2",     1.57079632679489661923),  // pi/2
        def_object("PI_4",     0.785398163397448309616), // pi/4
        def_object("1_PI",     0.318309886183790671538), // 1/pi
        def_object("2_PI",     0.636619772367581343076), // 2/pi
        def_object("2_SQRTPI", 1.12837916709551257390),  // 2/sqrt(pi)
        def_object("SQRT2",    1.41421356237309504880),  // sqrt(2)
        def_object("SQRT1_2",  0.707106781186547524401)  // 1/sqrt(2)
    ])

    static Parser expr_in_brackets(){
        return SomeParsers.between(SomeParsers.symbol('(' as char), SomeParsers.symbol(')' as char), { expr() })
    }

    static Parser factor0(){
        return expr_in_brackets()
            .orElse { funcs.apply { Calculator::expr_in_brackets() } }
            .orElse { consts }
            .orElse { SomeParsers.double_ }
    }

    static Parser factor(){
        return SomeParsers.chainr1(factor0(), pow)
    }

    static Parser term(){
        return SomeParsers.chainl1(factor(), mul.orElse { div }, false)
    }

    static Parser expr(){
        return SomeParsers.usign.flatMap { String sgn -> SomeParsers.chainl1 term(), add.orElse { sub }, sgn == "-" }
    }

    static Optional calc(String inp) {
        return expr().parse((inp))
    }
}
