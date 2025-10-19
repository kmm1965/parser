package kmm.parser;

import javafx.util.Pair;

public class Test {
    public static void main(String[] args){
        Parser<Double> expr = Calculator.expr();
        System.out.println(expr.parse("72 - 7 - (1 - 2) * 3").map(Pair::getKey));                  // 68.0
        System.out.println(expr.parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").map(Pair::getKey)); // -8.889
        System.out.println(expr.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").map(Pair::getKey));      // -0.3634085731426532
        System.out.println(expr.parse("sqr(sin(2)) + sqr(cos(1 + 1))").map(Pair::getKey));         // 1.0
        System.out.println(expr.parse("3 ^ 2 ^ 3").map(Pair::getKey));                             // 6561.0
        System.out.println(expr.parse("sin(- PI/4)").map(Pair::getKey));                           // -0.7071067811865475
        System.out.println(expr.parse(" E ^ PI ").map(Pair::getKey));                              // 23.140692632779263
        System.out.println(expr.parse(" PI ^ E ").map(Pair::getKey));                              // 22.45915771836104
    }
}
