#module MyParser

using Monads, Parsers
import Monads: mbind, (>>)

import Base: *, /, empty, skip

export Parser

# Correct an error in the Monads module
mbind(f::Function, m::Maybe)::Maybe = isa(m.value, Nothing) ? Maybe(nothing) : f(m.value)

and_then(m, f::Function) = mbind(f, m)

(/)(f::Function, m::Monad) = fmap(f, m)

(*)(mf::M, m::Function) where {M<:Monad} = @mdo M begin
    f <- mf
    f / m()
end

# Alternaive for Maybe
empty(::Type{Maybe})::Maybe = Maybe(nothing)

(|)(m::Maybe, f::Function)::Maybe = !isa(m.value, Nothing) ? m : f()

## Parser
struct Parser <: Monad
    unp :: Function # String -> Maybe{(T, String)}
end

my_parse(p::Parser, inp::String)::Maybe = p.unp(inp)

# Monad
mreturn(Parser, val)::Parser = Parser(inp::String -> Maybe((val, inp)))

# p >>= f = P $ \inp -> do (x, out) <- my_parse p inp; my_parse (f x) out
mbind(f::Function, p::Parser)::Parser = Parser(inp ->
    and_then(my_parse(p, inp), pair -> my_parse(f(first(pair)), last(pair))))

fmap(f::Function, p::Parser)::Parser = and_then(p, x -> mreturn(Parser, f(x)))

(*)(mf::Parser, fp::Function)::Parser = and_then(mf, f -> f / fp())

# Alternaive for Parser
# empty = P $ \_ -> Nothing
empty(::Type{Parser})::Parser = Parser(_ -> Maybe(nothing))

# p <|> q = P $ \inp -> my_parse p inp <|> my_parse q inp
(|)(p::Parser, q::Parser)::Parser = Parser(inp -> my_parse(p, inp) | () -> my_parse(q, inp))

anyChar = Parser(inp -> length(inp) == 0 ? Maybe(nothing) : Maybe((inp[1], inp[2:length(inp)])))

satisfy(f::Function)::Parser = and_then(anyChar, c -> f(c) ? mreturn(Parser, c) : empty(Parser))

some(p::Parser)::Parser = (c -> s -> c * s) / p * () -> many(p)

many(p::Parser)::Parser = some(p) | mreturn(Parser, "")

# space = many $ satisfy isSpace
space = many(satisfy(c::Char -> isspace(c)))

skip(p::Parser, fq::Function)::Parser = and_then(p, _ -> fq())

token(p::Parser)::Parser = and_then(space >> p, x -> skip(space, () -> mreturn(Parser, x)))

char(x::Char)::Parser = satisfy(c::Char -> c == x)

symbol(c::Char)::Parser = token(char(c))

natural = token(and_then(some(satisfy(c -> isdigit(c))), s -> mreturn(Parser, parse(Float64, s))))

# alnum = satisfy $ \c -> isAlphaNum c || c == '_'
alnum = satisfy(c -> isletter(c) || isdigit(c) || c == '_')

# name :: String -> Parser String
name(n::String)::Parser = and_then(some(alnum), s -> s == n ? mreturn(Parser, n) : empty(Parser))

# rest :: Parser a -> (a -> Parser a) -> Parser (a -> a -> a) -> a -> Parser a
rest(p::Function, ff::Function, op::Parser, a)::Parser =
    and_then(op, f::Function -> and_then(p(), b -> ff(f(a, b)))) | mreturn(Parser, a)

# chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
function chainl1(p::Parser, op::Parser)::Parser
    # rest_l a = rest p rest_l op a
    rest_l(a) = rest(() -> p, rest_l, op, a)

    and_then(p, a -> rest_l(a))
end

# chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
function chainr1(p::Parser, op::Parser)::Parser
    # scan = do { a <- p; rest_r a }
    scan() = and_then(p, a -> rest_r(a))

    # rest_r a = rest scan return op a
    rest_r(a) = rest(scan, b -> mreturn(Parser, b), op, a)

    scan()
end

between(open::Parser, close::Parser, fp::Function)::Parser =
    and_then(skip(open, fp), x -> skip(close, () -> mreturn(Parser, x)))

op2(c::Char, f::Function)::Parser = skip(symbol(c), () -> mreturn(Parser, f))

add = op2('+', (x, y) -> x + y)
sub = op2('-', (x, y) -> x - y)
mul = op2('*', (x, y) -> x * y)
div = op2('/', (x, y) -> x / y)
pow = op2('^', (x, y) -> x ^ y)

# def_object :: String -> a -> Parser a
def_object(n::String, x)::Parser = skip(name(n), () -> mreturn(Parser, x))

functions = [
    def_object("sin",   sin),
    def_object("cos",   cos),
    def_object("asin",  asin),
    def_object("acos",  acos),
    def_object("sinh",  sinh),
    def_object("cosh",  cosh),
    def_object("asinh", asinh),
    def_object("acosh", acosh),
    def_object("tan",   tan),
    def_object("log",   log),
    def_object("exp",   exp),
    def_object("sqrt",  sqrt),
    def_object("sqr",   x -> x * x)
  ]

func = foldl(|, functions)

m_E        = 2.71828182845904523536   # e
m_LOG2E    = 1.44269504088896340736   # log2(e)
m_LOG10E   = 0.434294481903251827651  # log10(e)
m_LN2      = 0.693147180559945309417  # ln(2)
m_LN10     = 2.30258509299404568402   # ln(10)
m_PI       = 3.14159265358979323846   # pi
m_PI_2     = 1.57079632679489661923   # pi/2
m_PI_4     = 0.785398163397448309616  # pi/4
m_1_PI     = 0.318309886183790671538  # 1/pi
m_2_PI     = 0.636619772367581343076  # 2/pi
m_2_SQRTPI = 1.12837916709551257390   # 2/sqrt(pi)
m_SQRT2    = 1.41421356237309504880   # sqrt(2)
m_SQRT1_2  = 0.707106781186547524401  # 1/sqrt(2)

constants = [
    def_object("E",        m_E),
    def_object("LOG2E",    m_LOG2E),
    def_object("LOG10E",   m_LOG10E),
    def_object("LN2",      m_LN2),
    def_object("LN10",     m_LN10),
    def_object("PI",       m_PI),
    def_object("PI_2",     m_PI_2),
    def_object("PI_4",     m_PI_4),
    def_object("1_PI",     m_1_PI),
    def_object("2_PI",     m_2_PI),
    def_object("2_SQRTPI", m_2_SQRTPI),
    def_object("SQRT2",    m_SQRT2),
    def_object("SQRT1_2",  m_SQRT1_2)
  ]

_const = foldl(|, constants)

# expr :: Parser Double
expr()::Parser = chainl1(term(), add | sub)
term()::Parser = chainl1(factor(), mul | div)
factor()::Parser = chainr1(factor0(), pow)
factor0()::Parser = expr_in_brackets() | func * expr_in_brackets | _const | natural
expr_in_brackets()::Parser = between(symbol('('), symbol(')'), expr)

#end

e = expr()

println(my_parse(e, "72 - 7 - (1 - 2) * 3"))             # 68.0
println(my_parse(e, "3^(1+1)^3"))                        # 6561.0
println(my_parse(e, "sin(1+1)"))                         # 0.9092974268256817
println(my_parse(e, "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")) # -0.3634085731426532
println(my_parse(e, "sqr(2 + 3)"))                       # 25.0
