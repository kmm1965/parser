package some_parsers_test

import (
    "testing"
    sp "some_parsers"
    "parser"
    "maybe"
    "math"
)

func TestAnyChar(t *testing.T){
    if sp.AnyChar().Parse("abc") != maybe.Just(parser.ParserPair[byte]{ 'a', "bc" }) {
        t.Errorf("anyChar.Parse(\"abc\") != Just(('a', \"bc\"))")
    }
    if sp.AnyChar().Parse("") != maybe.Nothing[parser.ParserPair[byte]]() {
        t.Errorf("anyChar.Parse(\"\") != Nothing")
    }
}

func TestSatisfy(t *testing.T){
    if sp.Satisfy(func (c byte) bool { return c == 'a' }).Parse("abc") != maybe.Just(parser.ParserPair[byte]{ 'a', "bc" }) {
        t.Errorf("Satisfy(c=='a') failed")
    }

    if sp.Satisfy(func (c byte) bool { return c == 'z' }).Parse("abc") != maybe.Nothing[parser.ParserPair[byte]]() {
        t.Errorf("Satisfy(c=='z') failed")
    }
}

func TestChar(t *testing.T){
    if sp.Char('a').Parse("abc") != maybe.Just(parser.ParserPair[byte]{ 'a', "bc" }) {
        t.Errorf("Char('a').Parse(\"abc\") != Just(('a', \"bc\"))")
    }
    if sp.Char('z').Parse("abc") != maybe.Nothing[parser.ParserPair[byte]]() {
        t.Errorf("Char('a').Parse(\"abc\") != Nothing")
    }
}


func TestEmptyString(t *testing.T){
    if sp.EmptyString().Parse("abc") != maybe.Just(parser.ParserPair[string]{ "", "abc" }) {
        t.Errorf("emptyString.Parse(\"abc\") != Just((\"\", \"abc\"))")
    }
}

func TestOptional(t *testing.T){
    if sp.Optional_c(sp.Char('1')).Parse("1234") != maybe.Just(parser.ParserPair[string]{ "1", "234" }) {
        t.Errorf("Optional_c(Char('1')).Parse(\"1234\") != Just((\"1\", \"234\"))")
    }
    if sp.Optional_c(sp.Char('1')).Parse("abc") != maybe.Just(parser.ParserPair[string]{ "", "abc" }) {
        t.Errorf("Optional_c(Char('1')).Parse(\"abc\") != Just((\"\", \"abc\"))")
    }
}


func TestSpaces(t *testing.T){
    if sp.Spaces().Parse("abc") != maybe.Just(parser.ParserPair[string]{ "", "abc" }) {
        t.Errorf("Spaces.Parse(\"abc\") != Just((\"\", \"abc\"))")
    }
    if sp.Spaces().Parse("  abc") != maybe.Just(parser.ParserPair[string]{ "  ", "abc" }) {
        t.Errorf("Spaces.Parse(\"abc\") != Just((\"  \", \"abc\"))")
    }
}

func TestSymbol(t *testing.T){
    if sp.Symbol('+').Parse(" + abc") != maybe.Just(parser.ParserPair[byte]{ '+', "abc" }) {
        t.Errorf("Symbol('+').Parse(\" + abc\") != Just(('+', \"abc\"))")
    }
    if sp.Symbol('+').Parse("abc") != maybe.Nothing[parser.ParserPair[byte]]() {
        t.Errorf("Symbol('+').Parse(\" + abc\") != Nothing")
    }
}

func TestAlnum(t *testing.T){
    if sp.Alnum().Parse("123abc  ") != maybe.Just(parser.ParserPair[byte]{ '1', "23abc  " }) {
        t.Errorf("Alnum.Parse(\"123abc  \") != Just(('1', \"23abc  \"))")
    }
    if sp.Alnum().Parse("_123abc  ") != maybe.Just(parser.ParserPair[byte]{ '_', "123abc  " }) {
        t.Errorf("Alnum.Parse(\"_123abc  \") != Just(('_', \"123abc  \"))")
    }
    if sp.Alnum().Parse("!@#$") != maybe.Nothing[parser.ParserPair[byte]]() {
        t.Errorf("Alnum.Parse(\"!@#$\") != Nothing")
    }
}

func TestName(t *testing.T){
    psin := sp.Name("sin")
    if psin.Parse(" sin ") != maybe.Just(parser.ParserPair[string]{ "sin", "" }) {
        t.Errorf("Name(\"sin\").Parse(\" sin \") != Just((\"sin\", \"\"))")
    }
    if psin.Parse("  sin  (1.)") != maybe.Just(parser.ParserPair[string]{ "sin", "(1.)" }) {
        t.Errorf("Name(\"sin\").Parse(\"  sin  (1.)\") != Just((\"sin\", \"(1.)\"))")
    }
    if psin.Parse("sinabc") != maybe.Nothing[parser.ParserPair[string]]() {
        t.Errorf("Name(\"sin\").Parse(\"sinabc\") != Nothing")
    }
}

func TestSign(t *testing.T){
    sign := sp.Sign()
    if sign.Parse("abc") != maybe.Just(parser.ParserPair[string]{ "", "abc" }) {
        t.Errorf("Sign.Parse(\"abc\") != Just((\"\", \"abc\"))")
    }
    if sign.Parse("+abc") != maybe.Just(parser.ParserPair[string]{ "+", "abc" }) {
        t.Errorf("Sign.Parse(\"+abc\") != Just((\"+\", \"abc\"))")
    }
    if sign.Parse("-abc") != maybe.Just(parser.ParserPair[string]{ "-", "abc" }) {
        t.Errorf("Sign.Parse(\"-abc\") != Just((\"-\", \"abc\"))")
    }

    usign := sp.Usign()
    if usign.Parse("abc") != maybe.Just(parser.ParserPair[string]{ "", "abc" }) {
        t.Errorf("Usign.Parse(\"abc\") != Just((\"\", \"abc\"))")
    }
    if usign.Parse(" + abc") != maybe.Just(parser.ParserPair[string]{ "+", "abc" }) {
        t.Errorf("Usign.Parse(\" + abc\") != Just((\"+\", \"abc\"))")
    }
    if usign.Parse(" - abc") != maybe.Just(parser.ParserPair[string]{ "-", "abc" }) {
        t.Errorf("Usign.Parse(\" - abc\") != Just((\"-\", \"abc\"))")
    }
}

func TestDigits(t *testing.T){
    digits := sp.Digits()
    if digits.Parse("123abc") != maybe.Just(parser.ParserPair[string]{ "123", "abc" }) {
        t.Errorf("Digits.Parse(\"123abc\") != Just((\"123\", \"abc\"))")
    }
    if digits.Parse("123  abc") != maybe.Just(parser.ParserPair[string]{ "123", "  abc" }) {
        t.Errorf("Digits.Parse(\"123  abc\") != Just((\"123\", \"  abc\"))")
    }
    if digits.Parse("abc") != maybe.Just(parser.ParserPair[string]{ "", "abc" }) {
        t.Errorf("Digits.Parse(\"abc\") != Just((\"\", \"abc\"))")
    }
}

func TestDouble(t *testing.T){
    double := sp.Double()
    if double.Parse(" 1 abc") != maybe.Just(parser.ParserPair[float64]{ 1., "abc" }) {
        t.Errorf("Double.Parse(\" 1 abc\") != Just((1., \"abc\"))")
    }
    if double.Parse(" 1. abc") != maybe.Just(parser.ParserPair[float64]{ 1., "abc" }) {
        t.Errorf("Double.Parse(\" 1. abc\") != Just((1., \"abc\"))")
    }
    if double.Parse(" 1.23 abc") != maybe.Just(parser.ParserPair[float64]{ 1.23, "abc" }) {
        t.Errorf("Double.Parse(\" 1.23 abc\") != Just((1.23, \"abc\"))")
    }
    if double.Parse(" .23 abc") != maybe.Just(parser.ParserPair[float64]{ 0.23, "abc" }) {
        t.Errorf("Double.Parse(\" .23 abc\") != Just((0.23, \"abc\"))")
    }
    if double.Parse(" + 1.23 abc") != maybe.Nothing[parser.ParserPair[float64]]() {
        t.Errorf("Double.Parse(\" + 1.23 abc\") != Nothing")
    }
    if double.Parse("1.23e10abc") != maybe.Just(parser.ParserPair[float64]{ 1.23e10, "abc" }) {
        t.Errorf("Double.Parse(\"1.23e10abc\") != Just((1.23e10, \"abc\"))")
    }
    if double.Parse("1.23e-10abc") != maybe.Just(parser.ParserPair[float64]{ 1.23e-10, "abc" }) {
        t.Errorf("Double.Parse(\"1.23e-10abc\") != Just((1.23e-10, \"abc\"))")
    }
    if double.Parse("abc") != maybe.Nothing[parser.ParserPair[float64]]() {
        t.Errorf("Double.Parse(\"abc\") != Nothing")
    }
}

func TestBetween(t *testing.T){
    expr := sp.Between(sp.Symbol('('), sp.Symbol(')'), sp.Double())
    if expr.Parse(" ( 123 ) abc") != maybe.Just(parser.ParserPair[float64]{ 123., "abc" }) {
        t.Errorf("expt..Parse(\" ( 123 ) abc\") != Just((123., \"abc\"))")
    }
    if expr.Parse(" ( 123 abc") != maybe.Nothing[parser.ParserPair[float64]]() {
        t.Errorf("expt..Parse(\" ( 123 abc\") != Nothing")
    }
    if expr.Parse(" 123 ) abc") != maybe.Nothing[parser.ParserPair[float64]]() {
        t.Errorf("expt..Parse(\" 123 ) abc\") != Nothing")
    }
}

func TestChainlr1(t *testing.T){
    add := parser.Skip(sp.Symbol('+'), parser.Pure(func (x float64, y float64) float64 { return x + y }))
    sub := parser.Skip(sp.Symbol('-'), parser.Pure(func (x float64, y float64) float64 { return x - y }))
    pow := parser.Skip(sp.Symbol('^'), parser.Pure(func (x float64, y float64) float64 { return math.Exp(y * math.Log(x)) }))
    double := sp.Double()
    pexpr := sp.Chainl1(double, parser.OrElse(add, sub), false);

    if pexpr.Parse("7abc") != maybe.Just(parser.ParserPair[float64]{ 7., "abc" }) {
        t.Errorf("expt.Parse(\"7abc\") != Just((7., \"abc\"))")
    }
    if pexpr.Parse(" 7 - 1 - 2 abc") != maybe.Just(parser.ParserPair[float64]{ 4., "abc" }) {
        t.Errorf("expt.Parse(\" 7 - 1 - 2 abc\") != Just((4., \"abc\"))")
    }
    if pexpr.Parse(" 7 - 1 + 2 - 3 abc") != maybe.Just(parser.ParserPair[float64]{ 5., "abc" }) {
        t.Errorf("expt.Parse(\" 7 - 1 + 2 - 3 abc\") != Just((5., \"abc\"))")
    }
    if pexpr.Parse("abc") != maybe.Nothing[parser.ParserPair[float64]]() {
        t.Errorf("expt.Parse(\"abc\") != Nothing")
    }

    if maybe.Map(sp.Chainr1(double, pow).Parse("3 ^ 2 ^ 3 abc"),
        func (pair parser.ParserPair[float64]) parser.ParserPair[float64] {
            return parser.ParserPair[float64]{ math.Round(pair.Key), pair.Value }
        }) != maybe.Just(parser.ParserPair[float64]{ 6561., "abc" }) {
        t.Errorf("Double.Parse(\"3 ^ 2 ^ 3 abc\") != Just((6561., \"abc\"))")
    }
}
