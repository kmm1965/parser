package calculator_test

import (
    "testing"
    c "calculator"
    "parser"
    "maybe"
    "math"
)

func TestFuncs(t *testing.T){
    if c.Calculate("sin(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Sin(2.0), "" }) {
        t.Errorf("calculate(\"sin(2.0)\") != Just((sin(2.0), \"\"))")
    }
    if c.Calculate("cos(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Cos(2.0), "" }) {
        t.Errorf("calculate(\"cos(2.0)\") != Just((cos(2.0), \"\"))")
    }
    if c.Calculate("asin(0.5)") != maybe.Just(parser.ParserPair[float64]{ math.Asin(0.5), "" }) {
        t.Errorf("calculate(\"asin(0.5)\") != Just((asin(0.5), \"\"))")
    }
    if c.Calculate("acos(0.5)") != maybe.Just(parser.ParserPair[float64]{ math.Acos(0.5), "" }) {
        t.Errorf("calculate(\"acos(0.5)\") != Just((acos(0.5), \"\"))")
    }
    if c.Calculate("sinh(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Sinh(2.0), "" }) {
        t.Errorf("calculate(\"sinh(2.0)\") != Just((sinh(2.0), \"\"))")
    }
    if c.Calculate("cosh(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Cosh(2.0), "" }) {
        t.Errorf("calculate(\"cosh(2.0)\") != Just((cosh(2.0), \"\"))")
    }
    if c.Calculate("asinh(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Asinh(2.0), "" }) {
        t.Errorf("calculate(\"asinh(2.0)\") != Just((asinh(2.0), \"\"))")
    }
    if c.Calculate("acosh(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Acosh(2.0), "" }) {
        t.Errorf("calculate(\"acosh(2.0)\") != Just((acosh(2.0), \"\"))")
    }
    if c.Calculate("tan(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Tan(2.0), "" }) {
        t.Errorf("calculate(\"tan(2.0)\") != Just((tan(2.0), \"\"))")
    }
    if c.Calculate("log(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Log(2.0), "" }) {
        t.Errorf("calculate(\"log(2.0)\") != Just((log(2.0), \"\"))")
    }
    if c.Calculate("log10(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Log10(2.0), "" }) {
        t.Errorf("calculate(\"log10(2.0)\") != Just((log10(2.0), \"\"))")
    }
    if c.Calculate("exp(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Exp(2.0), "" }) {
        t.Errorf("calculate(\"exp(2.0)\") != Just((exp(2.0), \"\"))")
    }
    if c.Calculate("sqrt(2.0)") != maybe.Just(parser.ParserPair[float64]{ math.Sqrt(2.0), "" }) {
        t.Errorf("calculate(\"sqrt(2.0)\") != Just((sqrt(2.0), \"\"))")
    }
    if c.Calculate("sqr(2.0)") != maybe.Just(parser.ParserPair[float64]{ 4.0, "" }) {
        t.Errorf("calculate(\"sqr(2.0)\") != Just((4.0, \"\"))")
    }
}

func TestConsts(t *testing.T){
    M_E := 2.71828182845904523536
    M_PI := 3.14159265358979323846
    if c.Calculate("E") != maybe.Just(parser.ParserPair[float64]{ M_E, "" }) {
        t.Errorf("calculate(\"E\") != Just((e, \"\"))")
    }
    if c.Calculate("LOG2E") != maybe.Just(parser.ParserPair[float64]{ 1 / math.Log(2.0), "" }) {
        t.Errorf("calculate(\"LOG2E\") != Just((1/log(2.0), \"\"))")
    }
    if c.Calculate("LOG10E") != maybe.Just(parser.ParserPair[float64]{ 0.4342944819032518, "" }) {
        t.Errorf("calculate(\"LOG10E\") != Just(( 1/log(10), \"\"))")
    }
//    if c.Calculate("LOG10E") != maybe.Just(parser.ParserPair[float64]{ 1 / math.Log(10), "" }) {
//        t.Errorf("calculate(\"LOG10E\") != Just(( 1/log(10), \"\"))")
//    } // 0.43429448190325182 and 0.43429448190325176

    if c.Calculate("LN2") != maybe.Just(parser.ParserPair[float64]{ math.Log(2.0), "" }) {
        t.Errorf("calculate(\"LN2\") != Just((log(2), \"\"))")
    }
    if c.Calculate("LN10") != maybe.Just(parser.ParserPair[float64]{ math.Log(10.0), "" }) {
        t.Errorf("calculate(\"LN10\") != Just((log(10), \"\"))")
    }
    if c.Calculate("PI") != maybe.Just(parser.ParserPair[float64]{ M_PI, "" }) {
        t.Errorf("calculate(\"PI\") != Just((pi, \"\"))")
    }
    if c.Calculate("PI_2") != maybe.Just(parser.ParserPair[float64]{ M_PI / 2, "" }) {
        t.Errorf("calculate(\"PI_2\") != Just((pi/2, \"\"))")
    }
    if c.Calculate("PI_4") != maybe.Just(parser.ParserPair[float64]{ M_PI / 4, "" }) {
        t.Errorf("calculate(\"PI_4\") != Just((pi/4, \"\"))")
    }
    if c.Calculate("1_PI") != maybe.Just(parser.ParserPair[float64]{ 1 / M_PI, "" }) {
        t.Errorf("calculate(\"1_PI\") != Just((1/pi, \"\"))")
    }
    if c.Calculate("2_PI") != maybe.Just(parser.ParserPair[float64]{ 2 / M_PI, "" }) {
        t.Errorf("calculate(\"2_PI\") != Just((2/pi, \"\"))")
    }
    if c.Calculate("2_SQRTPI") != maybe.Just(parser.ParserPair[float64]{ 2 / math.Sqrt(M_PI), "" }) {
        t.Errorf("calculate(\"2_SQRTPI\") != Just((2/sqrt(pi), \"\"))")
    }
    if c.Calculate("SQRT2") != maybe.Just(parser.ParserPair[float64]{ math.Sqrt(2), "" }) {
        t.Errorf("calculate(\"SQRT2\") != Just((sqrt(2), \"\"))")
    }
    if c.Calculate("SQRT1_2") != maybe.Just(parser.ParserPair[float64]{ math.Sqrt(0.5), "" }) {
        t.Errorf("calculate(\"SQRT1_2\") != Just((1/sqrt(2), \"\"))")
    }
}

func TestCalculator(t *testing.T){
    if c.Calculate("72 - 7 - (1 - 2) * 3") != maybe.Just(parser.ParserPair[float64]{ 68., "" }) {
        t.Errorf("calculate(\"72 - 7 - (1 - 2) * 3\") != Just((68., \"\"))")
    }
    if c.Calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") != maybe.Just(parser.ParserPair[float64]{ -8.889, "" }) {
        t.Errorf("calculate(\" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)\") != Just((-8.889, \"\"))")
    }
    if maybe.Map(c.Calculate("3^(1+1)^3"),
        func (pair parser.ParserPair[float64]) parser.ParserPair[float64] {
            return parser.ParserPair[float64]{ math.Round(pair.Key), pair.Value }
        }) != maybe.Just(parser.ParserPair[float64]{ 6561., "" }) {
        t.Errorf("Double.Parse(\"3^(1+1)^3\") != Just((6561., \"\"))")
    }
    if c.Calculate("sin(1+1)") != maybe.Just(parser.ParserPair[float64]{ math.Sin(2.), "" }) {
        t.Errorf("calculate(\"sin(1+1)\") != Just((sin(2.), \"\"))")
    }
    if c.Calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )") != maybe.Just(parser.ParserPair[float64]{ -0.36340857314265324, "" }) {
        t.Errorf("calculate(\"sin ( 2_SQRTPI * sqr ( 2 ) - 1 )\" != Just((-0.3634085731426532, \"\"))")
    }
    if c.Calculate("sqr(2 + 3)") != maybe.Just(parser.ParserPair[float64]{ 25., "" }) {
        t.Errorf("calculate(\"sqr(2 + 3)\" != Just((25., \"\"))")
    }
    if c.Calculate("sin(-PI/4)") != maybe.Just(parser.ParserPair[float64]{ -0.7071067811865475, "" }) {
        t.Errorf("calculate(\"sin(-PI/4)\" != Just((-0.7071067811865475, \"\"))")
    }
    if c.Calculate(" E ^ PI") != maybe.Just(parser.ParserPair[float64]{ 23.140692632779267, "" }) {
        t.Errorf("calculate(\" E ^ PI\" != Just((23.140692632779267, \"\"))")
    }
    if c.Calculate(" PI ^ E") != maybe.Just(parser.ParserPair[float64]{ 22.45915771836104, "" }) {
        t.Errorf("calculate(\" PI ^ E\" != Just((22.45915771836104, \"\"))")
    }
}
