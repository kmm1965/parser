package kmm.parser;

import javafx.util.Pair;

public class Test {
    public static void main(String[] args){
        System.out.println(new Calculator().expr()
            //.parse("72 - 7 - (1 - 2) * 3")
            //.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")
            //.parse("sqr(sin(2)) + sqr(cos(2))")
            .parse("3 ^ 2 ^ 3")
            .map(Pair::getKey));
    }
}
