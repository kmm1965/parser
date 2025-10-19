include("SomeParsers.jl")

module Calculator

using ..MonadParser, ..SomeParsers
import ..MonadParser: Parser, (|), mreturn, between, chainl1, chainr1
import ..SomeParsers: symbol, name, double

op2(c::Char, f::Function)::Parser = skip(symbol(c), () -> mreturn(Parser, f))

add = op2('+', (x, y) -> x + y)
sub = op2('-', (x, y) -> x - y)
mul = op2('*', (x, y) -> x * y)
div = op2('/', (x, y) -> x / y)
pow = op2('^', (x, y) -> x ^ y)

# def_object :: String -> a -> Parser a
def_object(n::String, x)::Parser = skip(name(n), () -> mreturn(Parser, x))

funcs =
    def_object("sin",   sin)   |
    def_object("cos",   cos)   |
    def_object("asin",  asin)  |
    def_object("acos",  acos)  |
    def_object("sinh",  sinh)  |
    def_object("cosh",  cosh)  |
    def_object("asinh", asinh) |
    def_object("acosh", acosh) |
    def_object("tan",   tan)   |
    def_object("log",   log)   |
    def_object("log10", log10) |
    def_object("exp",   exp)   |
    def_object("sqrt",  sqrt)  |
    def_object("sqr",   x -> x * x)

consts =
    def_object("E",        2.71828182845904523536)  |
    def_object("PI",       3.14159265358979323846)  |
    def_object("LOG2E",    1.44269504088896340736)  | # log2(e)
    def_object("LOG10E",   0.434294481903251827651) | # log10(e)
    def_object("LN2",      0.693147180559945309417) | # ln(2)
    def_object("LN10",     2.30258509299404568402)  | # ln(10)
    def_object("PI_2",     1.57079632679489661923)  | # pi/2
    def_object("PI_4",     0.785398163397448309616) | # pi/4
    def_object("1_PI",     0.318309886183790671538) | # 1/pi
    def_object("2_PI",     0.636619772367581343076) | # 2/pi
    def_object("2_SQRTPI", 1.12837916709551257390)  | # 2/sqrt(pi)
    def_object("SQRT2",    1.41421356237309504880)  | # sqrt(2)
    def_object("SQRT1_2",  0.707106781186547524401)   # 1/sqrt(2)

expr()::Parser = and_then(usign, sgn -> chainl1(term(), add | sub, sgn == "-"))
term()::Parser = chainl1(factor(), mul | div, false)
factor()::Parser = chainr1(factor0(), pow)
factor0()::Parser = expr_in_brackets() | funcs * expr_in_brackets | consts | double
expr_in_brackets()::Parser = between(symbol('('), symbol(')'), expr)

end # module Calculator
