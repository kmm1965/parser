package parser_test

import (
    "testing"
    "math"
    "strconv"
    "parser"
    "maybe"
)

func TestParserPure(t *testing.T){
    if parser.Pure(1).Parse("abc") != maybe.Just(parser.ParserPair[int]{ 1, "abc" }) {
        t.Errorf("Pure(1).Parse(\"abc\") != Just((1, \"abc\"))")
    }
    if parser.Pure(1.).Parse("abc") != maybe.Just(parser.ParserPair[float64]{ 1., "abc" }) {
        t.Errorf("Pure(1.).Parse(\"abc\") != Just((1., \"abc\"))")
    }
    if parser.Pure("1").Parse("abc") != maybe.Just(parser.ParserPair[string]{ "1", "abc" }) {
        t.Errorf("Pure(\"1\").Parse(\"abc\") != Just((\"1\", \"abc\"))")
    }
}

func TestParserFunctor(t *testing.T){
    fi := func(x int) string { return strconv.Itoa(x) }
    fd := func(x float64) string { return strconv.FormatFloat(x, 'f', 2, 64) }
    fs := func(s string) int {
        i, err := strconv.Atoi(s)
        if err != nil {
            t.Errorf("Error converting string to int %s:", err)
        }
        return i
    }

    if parser.Map(parser.Pure(1), fi).Parse("abc") != maybe.Just(parser.ParserPair[string]{ "1", "abc" }) {
        t.Errorf("Map(Pure(1), fi).Parse(\"abc\") != Just((\"1\", \"abc\"))")
    }
    if parser.Map(parser.Pure(1.), fd).Parse("abc") != maybe.Just(parser.ParserPair[string]{ "1.00", "abc" }) {
        t.Errorf("Map(Pure(1.), fd).Parse(\"abc\") != Just((\"1.00\", \"abc\"))")
    }
    if parser.Map(parser.Pure("1"), fs).Parse("abc") != maybe.Just(parser.ParserPair[int]{ 1, "abc" }) {
        t.Errorf("Map(Pure(\"1\"), fs).Parse(\"abc\") != Just((1, \"abc\"))")
    }

    if parser.Map(parser.Empty[int](), fi).Parse("abc") != maybe.Nothing[parser.ParserPair[string]]() {
        t.Errorf("Map(Empty, fi).Parse(\"abc\") != Nothing")
    }
    if parser.Map(parser.Empty[float64](), fd).Parse("abc") != maybe.Nothing[parser.ParserPair[string]]() {
        t.Errorf("Map(Empty, fd).Parse(\"abc\") != Nothing")
    }
    if parser.Map(parser.Empty[string](), fs).Parse("abc") != maybe.Nothing[parser.ParserPair[int]]() {
        t.Errorf("Map(Empty, fs).Parse(\"abc\") != Nothing")
    }
}

func TestParserApplicative(t *testing.T){
    psin := parser.Pure(func (x float64) float64 { return math.Sin(x) })
    psin_empty := parser.Empty[func(float64) float64]()
    fd := func() parser.Parser[float64]{ return parser.Pure(1.) }
    nf := func() parser.Parser[float64]{ return parser.Empty[float64]() }

    if parser.Apply(psin, fd).Parse("abc") != maybe.Just(parser.ParserPair[float64]{ math.Sin(1.), "abc" }) {
        t.Errorf("(psin <*> fd).Parse(\"abc\") != Just((sin(1.), \"abc\"))")
    }
    if parser.Apply(psin, nf).Parse("abc") != maybe.Nothing[parser.ParserPair[float64]]() {
        t.Errorf("(psin <*> nf).Parse(\"abc\") != Nothing")
    }

    if parser.Apply(psin_empty, fd).Parse("abc") != maybe.Nothing[parser.ParserPair[float64]]() {
        t.Errorf("(psin_empty <*> fd).Parse(\"abc\") != Nothing")
    }
    if parser.Apply(psin_empty, nf).Parse("abc") != maybe.Nothing[parser.ParserPair[float64]]() {
        t.Errorf("(psin_empty <*> nf).Parse(\"abc\") != Nothing")
    }
}

func TestParserMonad(t *testing.T){
    i1 := parser.Pure(1)
    iempty := parser.Empty[int]()

    eat := func(x int) parser.Parser[string] {
        return parser.Parser[string] { func(inp string) parser.ParserResult[string] {
            return maybe.Just(parser.ParserPair[string]{ strconv.Itoa(x) + inp, ""})
        }}
    }
    cancel := func(x int) parser.Parser[string] {
        return parser.Parser[string] { func(inp string) parser.ParserResult[string] {
            return maybe.Nothing[parser.ParserPair[string]]()
        }}
    }

    if parser.FlatMap(i1, eat).Parse("abc") != maybe.Just(parser.ParserPair[string]{ "1abc", "" }) {
        t.Errorf("(i1 >>= eat).Parse(\"abc\") != Just((\"1abc\", \"\")")
    }
    if parser.FlatMap(i1, cancel).Parse("abc") != maybe.Nothing[parser.ParserPair[string]]() {
        t.Errorf("(i1 >>= cancel).Parse(\"abc\") != Nothing")
    }

    if parser.FlatMap(iempty, eat).Parse("abc") != maybe.Nothing[parser.ParserPair[string]]() {
        t.Errorf("(iempty >>= cancel).Parse(\"abc\") != Nothing")
    }
    if parser.FlatMap(iempty, cancel).Parse("abc") != maybe.Nothing[parser.ParserPair[string]]() {
        t.Errorf("(iempty >>= cancel).Parse(\"abc\") != Nothing")
    }
}
