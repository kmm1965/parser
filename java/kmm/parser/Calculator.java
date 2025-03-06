package kmm.parser;

import java.util.function.UnaryOperator;
import java.util.function.BinaryOperator;

public class Calculator {

    private static Parser<BinaryOperator<Double>> op2(char c, BinaryOperator<Double> f){
        return  Parser.symbol(c).skip(() -> Parser.pure(f));
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
        return Parser.name(name).skip(() -> Parser.pure(value));
    }

    private static final Parser<UnaryOperator<Double>> [] functions = array(
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
    );

    private static final Parser<UnaryOperator<Double>> func = fold(functions).token();

    public static final double M_LOG2E    = 1.44269504088896340736;  // log2(e)
    public static final double M_LOG10E   = 0.434294481903251827651; // log10(e)
    public static final double M_LN2      = 0.693147180559945309417; // ln(2)
    public static final double M_LN10     = 2.30258509299404568402;  // ln(10)
    public static final double M_PI_2     = 1.57079632679489661923;  // pi/2
    public static final double M_PI_4     = 0.785398163397448309616; // pi/4
    public static final double M_1_PI     = 0.318309886183790671538; // 1/pi
    public static final double M_2_PI     = 0.636619772367581343076; // 2/pi
    public static final double M_2_SQRTPI = 1.12837916709551257390;  // 2/sqrt(pi)
    public static final double M_SQRT2    = 1.41421356237309504880;  // sqrt(2)
    public static final double M_SQRT1_2  = 0.707106781186547524401; // 1/sqrt(2)

    private static final Parser<Double> [] constants = array(
            def_object("E",        Math.E),
            def_object("PI",       Math.PI),
            def_object("LOG2E",    M_LOG2E),
            def_object("LOG10E",   M_LOG10E),
            def_object("LN2",      M_LN2),
            def_object("LN10",     M_LN10),
            def_object("PI_2",     M_PI_2),
            def_object("PI_4",     M_PI_4),
            def_object("1_PI",     M_1_PI),
            def_object("2_PI",     M_2_PI),
            def_object("2_SQRTPI", M_2_SQRTPI),
            def_object("SQRT2",    M_SQRT2),
            def_object("SQRT1_2",  M_SQRT1_2)
    );

    private static final Parser<Double> _const = fold(constants).token();

    private static final Parser<Character> br_open  = Parser.symbol('(');
    private static final Parser<Character> br_close = Parser.symbol(')');

    private static Parser<Double> expr_in_brackets(){
        return Parser.between(br_open, br_close, Calculator::expr);
    }

    private static Parser<Double> factor0(){
        return expr_in_brackets()
            .orElse(Parser.apply_u(func, Calculator::expr_in_brackets))
            .orElse(_const)
            .orElse(Parser._double);
    }

    private static Parser<Double> factor(){
        return factor0().chainr1(pow);
    }

    private static Parser<Double> term(){
        return factor().chainl1(mul.orElse(div));
    }

    public static Parser<Double> expr(){
        return term().chainl1(add.orElse(sub));
    }
}
