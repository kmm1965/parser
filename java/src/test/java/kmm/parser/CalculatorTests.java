package kmm.parser;

import kmm.utils.Pair;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTests {
    @Test
    public void test_funcs() {
        Parser<Double> pexpr = Calculator.expr();
        assertEquals(Optional.of(Pair.of(Math.sin(2.0), "")), pexpr.parse("sin(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.cos(2.0), "")), pexpr.parse("cos(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.asin(2.0), "")), pexpr.parse("asin(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.acos(2.0), "")), pexpr.parse("acos(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.sinh(2.0), "")), pexpr.parse("sinh(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.cosh(2.0), "")), pexpr.parse("cosh(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.tan(2.0), "")), pexpr.parse("tan(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.log(2.0), "")), pexpr.parse("log(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.log10(2.0), "")), pexpr.parse("log10(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.exp(2.0), "")), pexpr.parse("exp(2.0)"));
        assertEquals(Optional.of(Pair.of(Math.sqrt(2.0), "")), pexpr.parse("sqrt(2.0)"));
        assertEquals(Optional.of(Pair.of(4.0, "")), pexpr.parse("sqr(2.0)"));
    }

    @Test
    public void test_consts() {
        Parser<Double> pexpr = Calculator.expr();
        assertEquals(Optional.of(Pair.of(Math.PI, "")), pexpr.parse("PI"));
        assertEquals(Optional.of(Pair.of(Math.E, "")), pexpr.parse("E"));
        assertEquals(Optional.of(Pair.of(1 / Math.log(2.0), "")), pexpr.parse("LOG2E"));
        assertEquals(Optional.of(Pair.of(0.4342944819032518, "")), pexpr.parse("LOG10E"));
        assertEquals(Optional.of(Pair.of(Math.log(2.0), "")), pexpr.parse("LN2"));
        assertEquals(Optional.of(Pair.of(Math.log(10.0), "")), pexpr.parse("LN10"));
        assertEquals(Optional.of(Pair.of(Math.PI / 2, "")), pexpr.parse("PI_2"));
        assertEquals(Optional.of(Pair.of(Math.PI / 4, "")), pexpr.parse("PI_4"));
        assertEquals(Optional.of(Pair.of(1 / Math.PI, "")), pexpr.parse("1_PI"));
        assertEquals(Optional.of(Pair.of(2 / Math.PI, "")), pexpr.parse("2_PI"));
        assertEquals(Optional.of(Pair.of(2 / Math.sqrt(Math.PI), "")), pexpr.parse("2_SQRTPI"));
        assertEquals(Optional.of(Pair.of(Math.sqrt(2), "")), pexpr.parse("SQRT2"));
        assertEquals(Optional.of(Pair.of(Math.sqrt(0.5), "")), pexpr.parse("SQRT1_2"));
    }

    @Test
    public void test_calculator() {
        Parser<Double> pexpr = Calculator.expr();
        assertEquals(Optional.of(Pair.of(0.4342944819032518, "")), pexpr.parse("LOG10E"));
        assertEquals(Optional.of(Pair.of(68.0, "")), pexpr.parse("72 - 7 - (1 - 2) * 3"));
        assertEquals(Optional.of(Pair.of(-8.889, "")), pexpr.parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"));
        assertEquals(Optional.of(Pair.of(6561.0, "")), pexpr.parse("3^(1+1)^3"));
        assertEquals(Optional.of(Pair.of(Math.sin(2), "")), pexpr.parse("sin(1+1)"));
        assertEquals(Optional.of(Pair.of(-0.3634085731426532, "")), pexpr.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"));
        assertEquals(Optional.of(Pair.of(25.0, "")), pexpr.parse("sqr(2 + 3)"));
        assertEquals(Optional.of(Pair.of(Math.pow(Math.E, Math.PI), "")), pexpr.parse("E ^ PI"));
        assertEquals(Optional.of(Pair.of(Math.pow(Math.PI, Math.E), "")), pexpr.parse("PI ^ E"));
    }
}
