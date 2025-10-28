package test

import main.Parser

static test_Parser_pure() {
    assert(Parser.pure(1).parse("abc") == Optional.of(new Tuple2(1, "abc")))
    assert(Parser.pure(1.0).parse("abc") == Optional.of(new Tuple2(1.0, "abc")))
    assert(Parser.pure("1").parse("abc") == Optional.of(new Tuple2("1", "abc")))
}


static test_Parser_functor() {
    Closure fi = { int i -> i.toString() }
    Closure fd = { Double d -> d.toString() }
    Closure fs = { String s -> s.toInteger() }

    assert(Parser.pure(1).map(fi).parse("abc") == Optional.of(new Tuple2("1", "abc")))
    assert(Parser.pure(1.0).map(fd).parse("abc") == Optional.of(new Tuple2("1.0", "abc")))
    assert(Parser.pure("1").map(fs).parse("abc") == Optional.of(new Tuple2(1, "abc")))

    assert(Parser.empty().map(fi).parse("abc") == Optional.empty())
    assert(Parser.empty().map(fd).parse("abc") == Optional.empty())
    assert(Parser.empty().map(fs).parse("abc") == Optional.empty())
}

static test_Parser_applicative() {
    Parser psin = Parser.pure { Double x -> Math.sin(x) }
    Parser pempty = Parser.empty()
    Closure fd = { Parser.pure(1.0) }
    Closure nf = { Parser.empty() }

    assert(psin.apply(fd).parse("abc") == Optional.of(new Tuple2(Math.sin(1.0), "abc")))
    assert(psin.apply(nf).parse("abc") == Optional.empty())
    assert(pempty.apply(fd).parse("abc") == Optional.empty())
    assert(pempty.apply(nf).parse("abc") == Optional.empty())
}


static test_Parser_monad() {
    Parser i1 = Parser.pure(1)
    Parser pempty = Parser.empty()
    Closure eat = { int x -> new Parser(unp: { String inp -> Optional.of(new Tuple2(x.toString() + inp, "")) }) }
    Closure cancel = { int x -> new Parser(unp: { String inp -> Optional.empty() }) }

    assert(i1.flatMap(eat).parse("abc") == Optional.of(new Tuple2("1abc", "")))
    assert(i1.flatMap(cancel).parse("abc") == Optional.empty())
    assert(pempty.flatMap(eat).parse("abc") == Optional.empty())
    assert( pempty.flatMap(cancel).parse("abc") == Optional.empty())
}

test_Parser_pure()
test_Parser_functor()
test_Parser_applicative()
test_Parser_monad()