package kmm.parser;

import javafx.util.Pair;

public class Test {
    public static void main(String[] args){
        Parser<Double> expr = Calculator.expr();
        System.out.println(expr.parse("72 - 7 - (1 - 2) * 3").map(Pair::getKey));
        System.out.println(expr.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").map(Pair::getKey));
        System.out.println(expr.parse("sqr(sin(2)) + sqr(cos(1 + 1))").map(Pair::getKey));
        System.out.println(expr.parse("3 ^ 2 ^ 3").map(Pair::getKey));
    }
}
