package calculator_test

import (
    "testing"
    c "calculator"
    "parser"
    "maybe"
    "math"
)

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
