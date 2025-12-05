package kmm.parser;

import kmm.utils.Pair;

public class Test {
    static void main(){
        Parser<Double> pexpr = Calculator.expr();
        System.out.println(pexpr.parse("72 - 7 - (1 - 2) * 3").map(Pair::first));                  // 68.0
        System.out.println(pexpr.parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").map(Pair::first)); // -8.889
        System.out.println(pexpr.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").map(Pair::first));      // -0.3634085731426532
        System.out.println(pexpr.parse("sqr(sin(2)) + sqr(cos(1 + 1))").map(Pair::first));         // 1.0
        System.out.println(pexpr.parse("3 ^ 2 ^ 3").map(Pair::first));                             // 6561.0
        System.out.println(pexpr.parse("sin(- PI/4)").map(Pair::first));                           // -0.7071067811865475
        System.out.println(pexpr.parse(" E ^ PI ").map(Pair::first));                              // 23.140692632779263
        System.out.println(pexpr.parse(" PI ^ E ").map(Pair::first));                              // 22.45915771836104
    }
}
