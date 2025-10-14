import std.typecons: Tuple, tuple;

import maybe;

class Parser(T)
{
    alias result_t = Maybe!(Tuple!(T, string));
    alias func_t = result_t delegate(string) pure;

    func_t fun;

    this(func_t fun) pure {
        this.fun = fun;
    }

    result_t parse(string inp) const pure {
        return fun(inp);
    }

    auto opUnary(string op)() const pure
    {
        static if(op == "*"){ // many
            return this.many;
        } else static if(op == "+"){ // some
            return this.some;
        } else static if(op == "-"){ // optional
            static if(is(T == string))
                return this.optional_s;
            else return this.optional_c;
        } else static if(op == "~"){ // token
            return this.token;
        }
    }

    auto opBinaryRight(string op, Func)(Func func) const pure
    {
        static if(op == "/"){ // Functor
            static assert(is(typeof(func(T.init))));
            return this.transform!func;
        }
    }

    auto opBinary(string op, Arg)(Arg arg) const pure {
        static if(op == "*"){ // Applicative
            return this.and_then!(f => f / invoke(arg));
        } else static if(op == ">>"){ // Monad
            return this.and_then!(_ => invoke(arg));
        } else static if(op == "|"){ // Alternative
            import optional: or;
            return new Parser!T(inp => this.parse(inp).or(invoke(arg).parse(inp)));
        }
    }

    // Applicative
    static Parser!T _pure(T x) pure {
        return new Parser!T(inp => Just(tuple(x, inp)));
    }

    // Alternative
    static Parser!T empty() pure {
        return new Parser!T(_ => Nothing!(Tuple!(T, string)));
    }

    static Parser!T init() pure {
        return empty();
    }
}

/// Checks if T is a parser type
template isParser(T){
    import std.traits: isInstanceOf;
    enum isParser = isInstanceOf!(Parser, T);
}

@("isParser unit test")
nothrow unittest {
    static assert(isParser!(Parser!int));
    static assert(!isParser!int);
}

/// Returns the target type of a parser.
template ParserTarget(T) if (isParser!T) {
    import std.traits: TemplateArgsOf;
    alias ParserTarget = TemplateArgsOf!T[0];
}

@("ParserTarget unit test")
nothrow unittest {
    class C {}
    struct S {}

    import std.meta: AliasSeq;
    foreach (T; AliasSeq!(int, string, S, C, int[], string[], S[], C[])){
        alias ST = shared T;

        static assert(is(ParserTarget!(Parser!T) == T));
        static assert(is(ParserTarget!(Parser!ST) == ST));
    }
}

// Functor
auto transform(alias func, T)(const Parser!T p) pure
    if (is (typeof(func(T.init))))
{
    return p.and_then!(x => Parser_pure(func(x)));
}

@("Parser Functor unit tests")
unittest {
    import std.conv: to;

    auto fi = (int x)    => x.to!string;
    auto fd = (double x) => x.to!string;
    auto fs = (string s) => s.to!int;

    assert((fi / Parser_pure(1)).parse("abc") == Just(tuple("1", "abc")));
    assert((fd / Parser_pure(1.)).parse("abc") == Just(tuple("1", "abc")));
    assert((fs / Parser_pure("1")).parse("abc") == Just(tuple(1, "abc")));

    assert((fi / Parser!int.empty).parse("abc") == Nothing!(Tuple!(string, string)));
    assert((fd / Parser!double.empty).parse("abc") == Nothing!(Tuple!(string, string)));
    assert((fs / Parser!string.empty).parse("abc") == Nothing!(Tuple!(int, string)));
}

// Applicative
Parser!T Parser_pure(T)(T x) pure {
    return Parser!T._pure(x);
}

@("Parser_pure unit tests")
unittest {
    assert(Parser_pure(1).parse("abc") == Just(tuple(1, "abc")));
    assert(Parser_pure(1.).parse("abc") == Just(tuple(1., "abc")));
    assert(Parser_pure("1").parse("abc") == Just(tuple("1", "abc")));
}

@("Parser Applicative unit tests")
unittest {
    import std.math: sin;

    auto psin = Parser_pure((double x) => sin(x));
    auto psin_empty = Parser!(ParserTarget!(typeof(psin))).empty;
    auto fd = () => Parser_pure(1.);
    auto nf = () => Parser!double.empty;

    assert((psin * fd).parse("abc") == Just(tuple(sin(1.), "abc")));
    assert((psin * nf).parse("abc") == Nothing!(Tuple!(double, string)));

    assert((psin_empty * fd).parse("abc") == Nothing!(Tuple!(double, string)));
    assert((psin_empty * nf).parse("abc") == Nothing!(Tuple!(double, string)));
}

// Monad
auto and_then(alias func, T)(const Parser!T p) pure
    if (isParser!(typeof(func(T.init))))
{
    import maybe: and_then;

    alias U = ParserTarget!(typeof(func(T.init)));
    auto f = (Tuple!(T, string) pair) => func(pair[0]).parse(pair[1]);
    return new Parser!U(inp => p.parse(inp).and_then!f);
}

@("Parser and_then unit tests")
unittest {
    import std.conv: to;

    auto i1 = Parser_pure(1);
    auto iempty = Parser!int.empty;

    auto eat    = (int x) => new Parser!string(inp => Just(tuple(x.to!string ~ inp, "")));
    auto cancel = (int x) => new Parser!string(inp => Nothing!(Tuple!(string, string)));

    assert(i1.and_then!eat.parse("abc") == Just(tuple("1abc", "")));
    assert(i1.and_then!cancel.parse("abc") == Nothing!(Tuple!(string, string)));

    assert(iempty.and_then!eat.parse("abc") == Nothing!(Tuple!(string, string)));
    assert(iempty.and_then!cancel.parse("abc") == Nothing!(Tuple!(string, string)));
}

Parser!char anyChar() pure {
    return new Parser!char(inp => inp.length > 0 ?
        Just(tuple(cast(char)inp[0], inp[1..$])) :
        Nothing!(Tuple!(char, string)));
}

@("anyChar unit test")
unittest {
    assert(anyChar.parse("abc") == Just(tuple('a', "bc")));
    assert(anyChar.parse("") == Nothing!(Tuple!(char, string)));
}

Parser!string empty_string() pure {
    return Parser_pure(string.init);
}

@("empty_string unit test")
unittest {
    assert(empty_string.parse("abc") == Just(tuple("", "abc")));
}

Parser!string optional_s(const Parser!string p) pure {
    return p | empty_string;
}

Parser!string optional_c(const Parser!char p) pure {
    import std.conv: to;

    return optional_s(((char c) => c.to!string) / p);
}

@("optional_s unit test")
unittest {
    //assert(name("sin").optional_s.parse(" sin abc") == Just(tuple("sin", "abc")));
    //assert(name("sin").optional_s.parse("abc") == Just(tuple("", "abc")));
    assert(char_('1').optional_c.parse("1234") == Just(tuple("1", "234")));
    assert(char_('1').optional_c.parse("abc") == Just(tuple("", "abc")));
    assert((-char_('1')).parse("1234") == Just(tuple("1", "234")));
    assert((-char_('1')).parse("abc") == Just(tuple("", "abc")));
}

Parser!char satisfy(alias pred)() pure
    if (is (typeof(pred(char.init)) == bool))
{
    return anyChar.and_then!(c => pred(c) ? Parser_pure(c) : Parser!char.empty);
}

@("satisfy unit test")
unittest {
    assert(satisfy!(c => c == 'a').parse("abc") == Just(tuple('a', "bc")));
    assert(satisfy!(c => c == 'z').parse("abc") == Nothing!(Tuple!(char, string)));
}

Parser!char char_(char c) pure {
    return satisfy!(x => x == c);
}

@("char_ unit test")
unittest {
    assert(char_('a').parse("abc") == Just(tuple('a', "bc")));
    assert(char_('z').parse("abc") == Nothing!(Tuple!(char, string)));
}

Parser!string some(const Parser!char p) pure {
    return ((char c) => (string s) => c ~ s) / p * (() => *p);
}

Parser!string many(const Parser!char p) pure {
    return +p | empty_string;
}

Parser!string spaces() pure {
    import std.ascii: isWhite;
    return *satisfy!(c => isWhite(c));
}

@("spaces unit test")
unittest {
    assert(spaces.parse("abc") == Just(tuple("", "abc")));
    assert(spaces.parse("  abc") == Just(tuple("  ", "abc")));
}

bool isCallable(Arg)() pure
{
    static if(is(typeof(Arg())))
        return true;
    else return false;
}

auto invoke(Arg)(Arg arg) pure
{
    static if(isCallable!Arg)
        return arg();
    else return arg;
}

Parser!(ParserTarget!(typeof(invoke(fp)))) between(alias fp, Open, Close)(const Parser!Open open, const Parser!Close close) pure
    if (isParser!(typeof(invoke(fp))))
{
    return (open >> fp).and_then!(x => close >> Parser_pure(x));
}

Parser!T token(T)(const Parser!T p) pure {
    return between!p(spaces, spaces);
}

Parser!T rest(T, Op, alias fval, alias ff)(const Parser!Op op, T a) pure
    if (is (typeof(Op.init(T.init, T.init)) == T) &&
        isParser!(typeof(fval()))     && is(ParserTarget!(typeof(fval())) == T) &&
        isParser!(typeof(ff(T.init))) && is(ParserTarget!(typeof(ff(T.init))) == T))
{
    return op.and_then!(f => fval().and_then!(b => ff(f(a, b)))) | () => Parser_pure(a);
}

Parser!T rest_l(T, Op)(const Parser!T p, const Parser!Op op, T a) pure
    if (is (typeof(Op.init(T.init, T.init)) == T))
{
    return op.rest!(T, Op, () => p, b => p.rest_l(op, b))(a);
}

Parser!T rest_r(T, Op)(const Parser!T p, const Parser!Op op, T a) pure
    if (is (typeof(Op.init(T.init, T.init)) == T))
{
    return op.rest!(T, Op, () => p.chainr1(op), b => Parser_pure(b))(a);
}

Parser!T chainl1(T, Op)(const Parser!T p, const Parser!Op op, bool negate_first) pure
    if (is (typeof(Op.init(T.init, T.init)) == T))
{
    return p.and_then!(a => p.rest_l(op, negate_first ? -a : a));
}

Parser!T chainr1(T, Op)(const Parser!T p, const Parser!Op op) pure
    if (is (typeof(Op.init(T.init, T.init)) == T))
{
    return p.and_then!(a => p.rest_r(op, a));
}
