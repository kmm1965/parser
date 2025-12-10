package kmm.parser;

import kmm.utils.Pair;

import java.util.Optional;
import java.util.function.UnaryOperator;
import java.util.function.BinaryOperator;
import java.util.stream.Stream;

import static kmm.parser.Parser.*;
import static kmm.parser.SomeParsers.*;

public class Calculator {

    private static Parser<BinaryOperator<Double>> op2(char c, BinaryOperator<Double> f){
        return symbol(c).skip(() -> Parser.pure(f));
    }

    private static final Parser<BinaryOperator<Double>> add = op2('+', Double::sum);
    private static final Parser<BinaryOperator<Double>> sub = op2('-', (x, y) -> x - y);
    private static final Parser<BinaryOperator<Double>> mul = op2('*', (x, y) -> x * y);
    private static final Parser<BinaryOperator<Double>> div = op2('/', (x, y) -> x / y);
    private static final Parser<BinaryOperator<Double>> pow = op2('^', Math::pow);

    private static <A> Parser<A> guard(boolean b, A value){
        return b ? Parser.pure(value) : Parser.empty();
    }

    private static Parser<UnaryOperator<Double>> guardOp(boolean b, UnaryOperator<Double> op){
        return guard(b, op);
    }

    private static final Parser<UnaryOperator<Double>> functions = identifier.flatMap(n -> Stream.of(
            guardOp(n.equals("sin"),   Math::sin),
            guardOp(n.equals("cos"),   Math::cos),
            guardOp(n.equals("asin"),  Math::asin),
            guardOp(n.equals("acos"),  Math::acos),
            guardOp(n.equals("sinh"),  Math::sinh),
            guardOp(n.equals("cosh"),  Math::cosh),
            guardOp(n.equals("tan"),   Math::tan),
            guardOp(n.equals("log"),   Math::log),
            guardOp(n.equals("log10"), Math::log10),
            guardOp(n.equals("exp"),   Math::exp),
            guardOp(n.equals("sqrt"),  Math::sqrt),
            guardOp(n.equals("sqr"),   x -> x * x)
        ).reduce(Parser.empty(), Parser::orElse));
    
    private static final Parser<Double> constants = identifier.flatMap(n -> Stream.of(
            guard(n.equals("E"),        Math.E),
            guard(n.equals("PI"),       Math.PI),
            guard(n.equals("LOG2E"),    1.44269504088896340736),  // log2(e)
            guard(n.equals("LOG10E"),   0.434294481903251827651), // log10(e)
            guard(n.equals("LN2"),      0.693147180559945309417), // ln(2)
            guard(n.equals("LN10"),     2.30258509299404568402),  // ln(10)
            guard(n.equals("PI_2"),     1.57079632679489661923),  // pi/2
            guard(n.equals("PI_4"),     0.785398163397448309616), // pi/4
            guard(n.equals("1_PI"),     0.318309886183790671538), // 1/pi
            guard(n.equals("2_PI"),     0.636619772367581343076), // 2/pi
            guard(n.equals("2_SQRTPI"), 1.12837916709551257390),  // 2/sqrt(pi)
            guard(n.equals("SQRT2"),    1.41421356237309504880),  // sqrt(2)
            guard(n.equals("SQRT1_2"),  0.707106781186547524401)  // 1/sqrt(2)
        ).reduce(Parser.empty(), Parser::orElse));

    private static Parser<Double> exprInBrackets(){
        return between(symbol('('), symbol(')'), Calculator::expr);
    }

    private static Parser<Double> factor0(){
        return exprInBrackets()
            .or(() -> apply_u(functions, Calculator::exprInBrackets))
            .orElse(constants)
            .orElse(double_);
    }

    private static Parser<Double> factor(){
        return factor0().chainr1(pow);
    }

    private static Parser<Double> term(){
        return chainl1(factor(), mul.orElse(div));
    }

    public static Parser<Double> expr(){
        return usign.flatMap(sgn -> chainl1(term(), add.orElse(sub), sgn.equals("-")));
    }

    @SuppressWarnings("unused")
    public static Optional<Pair<Double, String>> calculate(String inp){
        return expr().parse(inp);
    }
}
