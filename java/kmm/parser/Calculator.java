package kmm.parser;

import java.util.function.UnaryOperator;
import java.util.function.BinaryOperator;

import static kmm.parser.SomeParsers.usign;

public class Calculator {

    private static Parser<BinaryOperator<Double>> op2(char c, BinaryOperator<Double> f){
        return  SomeParsers.symbol(c).skip(() -> Parser.pure(f));
    }
    private static final Parser<BinaryOperator<Double>> add = op2('+', Double::sum);
    private static final Parser<BinaryOperator<Double>> sub = op2('-', (x, y) -> x - y);
    private static final Parser<BinaryOperator<Double>> mul = op2('*', (x, y) -> x * y);
    private static final Parser<BinaryOperator<Double>> div = op2('/', (x, y) -> x / y);
    private static final Parser<BinaryOperator<Double>> pow = op2('^', Math::pow);

    @SafeVarargs
    private static <A> A[] array(final A... values){
        return values;
    }

    public static double sqr(double x){
        return x * x;
    }

    private static <A> Parser<A> fold(Parser<A>[] parsers)
    {
        Parser<A> p0 = Parser.empty();
        for(Parser<A> p : parsers)
            p0 = p0.orElse(p);
        return p0;
    }

    private static <A> Parser<A> def_object(String name, A value){
        return SomeParsers.name(name).skip(() -> Parser.pure(value));
    }

    private static final Parser<UnaryOperator<Double>> funcs = fold(array(
            def_object("sin",   Math::sin),
            def_object("cos",   Math::cos),
            def_object("asin",  Math::asin),
            def_object("acos",  Math::acos),
            def_object("sinh",  Math::sinh),
            def_object("cosh",  Math::cosh),
            def_object("tan",   Math::tan),
            def_object("log",   Math::log),
            def_object("log10", Math:: log10),
            def_object("exp",   Math::exp),
            def_object("sqrt",  Math::sqrt),
            def_object("sqr",   Calculator::sqr)
    ));

    private static final Parser<Double> consts = fold(array(
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
    ));

    private static Parser<Double> expr_in_brackets(){
        return Parser.between(SomeParsers.symbol('('), SomeParsers.symbol(')'), Calculator::expr);
    }

    private static Parser<Double> factor0(){
        return expr_in_brackets()
            .orElse(Parser.apply_u(funcs, Calculator::expr_in_brackets))
            .orElse(consts)
            .orElse(SomeParsers.double_);
    }

    private static Parser<Double> factor(){
        return factor0().chainr1(pow);
    }

    private static Parser<Double> term(){
        return Parser.chainl1(factor(), mul.orElse(div));
    }

    public static Parser<Double> expr(){
        return usign.flatMap(sgn -> Parser.chainl1(term(), add.orElse(sub), sgn.equals("-")));
    }
}
