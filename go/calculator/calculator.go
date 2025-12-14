package calculator

import (
    sp "some_parsers"
    "parser"
    "math"
)

func Op2(c byte, f func(float64, float64) float64) parser.Parser[func(float64, float64) float64] {
    return parser.SkipGet(sp.Symbol(c),
        func () parser.Parser[func(float64, float64) float64] { return parser.Pure(f) })
}

func Add() parser.Parser[func(float64, float64) float64] {
    return Op2('+', func (x float64, y float64) float64 { return x + y })
}

func Sub() parser.Parser[func(float64, float64) float64] {
    return Op2('-', func (x float64, y float64) float64 { return x - y })
}

func Mul() parser.Parser[func(float64, float64) float64] {
    return Op2('*', func (x float64, y float64) float64 { return x * y })
}

func Div() parser.Parser[func(float64, float64) float64] {
    return Op2('/', func (x float64, y float64) float64 { return x / y })
}

func Pow() parser.Parser[func(float64, float64) float64] {
    return Op2('^', func (x float64, y float64) float64 { return math.Exp(y * math.Log(x)) })
}

func fold[A any] (l []parser.Parser[A]) parser.Parser[A] {
    p := parser.Empty[A]()
    for _, q := range l {
        p = parser.OrElse(p, q)
    }
    return p
}

func guard[A any] (b bool, x A) parser.Parser[A] {
    if b {
        return parser.Pure(x)
    } else {
        return parser.Empty[A]()
    }
}

func funcs () parser.Parser[func(float64) float64] {
    return parser.FlatMap(sp.Identifier(), func (n string) parser.Parser[func(float64) float64] {
        return fold([]parser.Parser[func(float64) float64] {
            guard(n == "sin",   math.Sin),
            guard(n == "cos",   math.Cos),
            guard(n == "asin",  math.Asin),
            guard(n == "acos",  math.Acos),
            guard(n == "sinh",  math.Sinh),
            guard(n == "cosh",  math.Cosh),
            guard(n == "asinh", math.Asinh),
            guard(n == "acosh", math.Acosh),
            guard(n == "tan",   math.Tan),
            guard(n == "atan",  math.Atan),
            guard(n == "log",   math.Log),
            guard(n == "log10", math.Log10),
            guard(n == "exp",   math.Exp),
            guard(n == "sqrt",  math.Sqrt),
            guard(n == "sqr",   func (x float64) float64 { return x * x }),
        })
    })
}

func consts () parser.Parser[float64] {
    return parser.FlatMap(sp.Identifier(), func (n string) parser.Parser[float64] {
        return fold([]parser.Parser[float64] {
            guard(n == "E",        2.71828182845904523536),
            guard(n == "PI",       3.14159265358979323846),
            guard(n == "LOG2E",    1.44269504088896340736),  // log2(e)
            guard(n == "LOG10E",   0.434294481903251827651), // log10(e)
            guard(n == "LN2",      0.693147180559945309417), // ln(2)
            guard(n == "LN10",     2.30258509299404568402),  // ln(10)
            guard(n == "PI_2",     1.57079632679489661923),  // pi/2
            guard(n == "PI_4",     0.785398163397448309616), // pi/4
            guard(n == "1_PI",     0.318309886183790671538), // 1/pi
            guard(n == "2_PI",     0.636619772367581343076), // 2/pi
            guard(n == "2_SQRTPI", 1.12837916709551257390),  // 2/sqrt(pi)
            guard(n == "SQRT2",    1.41421356237309504880),  // sqrt(2)
            guard(n == "SQRT1_2",  0.707106781186547524401), // 1/sqrt(2)
        })
    })
}

func Expr() parser.Parser[float64] {
    return parser.FlatMap(sp.Usign(), func (sgn string) parser.Parser[float64] {
        return sp.Chainl1(Term(), parser.OrElse(Add(), Sub()), sgn == "-")
    })
}

func Term() parser.Parser[float64] {
    return sp.Chainl1(Factor(), parser.OrElse(Mul(), Div()), false)
}

func Factor() parser.Parser[float64] {
    return sp.Chainr1(Factor0(), Pow())
}

func Factor0() parser.Parser[float64] {
    return parser.OrElseGet(parser.OrElseGet(parser.OrElseGet(
        ExprInBrackets(),
        func () parser.Parser[float64] { return parser.Apply(funcs(), ExprInBrackets) }),
        func () parser.Parser[float64] { return consts() }),
        func () parser.Parser[float64] { return sp.Double() })
}

func ExprInBrackets() parser.Parser[float64] {
    return sp.BetweenGet(sp.Symbol('('), sp.Symbol(')'), Expr)
}

func Calculate(s string) parser.ParserResult[float64] {
    return Expr().Parse(s)
}
