include("SomeParsers.jl")

module Calculator

using ..MonadParser, ..SomeParsers
import ..MonadParser: Parser, (|), mreturn, between, chainl1, chainr1
import ..SomeParsers: symbol, identifier, double

op2(c::Char, f::Function)::Parser = skip(symbol(c), () -> mreturn(Parser, f))

add = op2('+', (x, y) -> x + y)
sub = op2('-', (x, y) -> x - y)
mul = op2('*', (x, y) -> x * y)
div = op2('/', (x, y) -> x / y)
pow = op2('^', (x, y) -> x ^ y)

guard(b::Bool, x)::Parser = b ? mreturn(Parser, x) : empty(Parser)

funcs = and_then(identifier, n ->
    guard(n == "sin",   sin)   |
    guard(n == "cos",   cos)   |
    guard(n == "asin",  asin)  |
    guard(n == "acos",  acos)  |
    guard(n == "sinh",  sinh)  |
    guard(n == "cosh",  cosh)  |
    guard(n == "asinh", asinh) |
    guard(n == "acosh", acosh) |
    guard(n == "tan",   tan)   |
    guard(n == "log",   log)   |
    guard(n == "log10", log10) |
    guard(n == "exp",   exp)   |
    guard(n == "sqrt",  sqrt)  |
    guard(n == "sqr",   x -> x * x))

consts = and_then(identifier, n ->
    guard(n == "E",        2.71828182845904523536)  |
    guard(n == "PI",       3.14159265358979323846)  |
    guard(n == "LOG2E",    1.44269504088896340736)  | # log2(e)
    guard(n == "LOG10E",   0.434294481903251827651) | # log10(e)
    guard(n == "LN2",      0.693147180559945309417) | # ln(2)
    guard(n == "LN10",     2.30258509299404568402)  | # ln(10)
    guard(n == "PI_2",     1.57079632679489661923)  | # pi/2
    guard(n == "PI_4",     0.785398163397448309616) | # pi/4
    guard(n == "1_PI",     0.318309886183790671538) | # 1/pi
    guard(n == "2_PI",     0.636619772367581343076) | # 2/pi
    guard(n == "2_SQRTPI", 1.12837916709551257390)  | # 2/sqrt(pi)
    guard(n == "SQRT2",    1.41421356237309504880)  | # sqrt(2)
    guard(n == "SQRT1_2",  0.707106781186547524401))  # 1/sqrt(2)

expr()::Parser = and_then(usign, sgn -> chainl1(term(), add | sub, sgn == "-"))
term()::Parser = chainl1(factor(), mul | div, false)
factor()::Parser = chainr1(factor0(), pow)
factor0()::Parser = expr_in_brackets() | funcs * expr_in_brackets | consts | double
expr_in_brackets()::Parser = between(symbol('('), symbol(')'), expr)

end # module Calculator
