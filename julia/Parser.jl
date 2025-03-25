module MonadParser

using Monads, Parsers
import Monads: mbind, (>>)

import Base: *, /, |, empty, skip

export Parser, expr, my_parse, mreturn, and_then, satisfy, optional_s, optional_c, between, token, some, many, chainl1, chainr1

# Correct an error in the Monads module
mbind(f::Function, m::Maybe)::Maybe = isa(m.value, Nothing) ? Maybe(nothing) : f(m.value)

and_then(m, f::Function) = mbind(f, m)

(/)(f::Function, m::Monad) = fmap(f, m)

(*)(mf::M, m::Function) where {M<:Monad} = and_then(mf, f -> f / m())
#@mdo M begin
#    f <- mf
#    f / m()
#end

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

# Alternaive for Parser
# empty = P $ \_ -> Nothing
empty(::Type{Parser})::Parser = Parser(_ -> Maybe(nothing))

# p <|> q = P $ \inp -> my_parse p inp <|> my_parse q inp
(|)(p::Parser, q::Parser)::Parser = Parser(inp -> my_parse(p, inp) | () -> my_parse(q, inp))

anyChar = Parser(inp -> length(inp) == 0 ? Maybe(nothing) : Maybe((inp[1], inp[2:length(inp)])))

satisfy(f::Function)::Parser = and_then(anyChar, c -> f(c) ? mreturn(Parser, c) : empty(Parser))

optional_s(p::Parser)::Parser = p | mreturn(Parser, "")

optional_c(p::Parser)::Parser = optional_s((c -> string(c)) / p)

some(p::Parser)::Parser = (c -> s -> c * s) / p * () -> many(p)

many(p::Parser)::Parser = optional_s(some(p))

# spaces = many $ satisfy isSpace
spaces = many(satisfy(c::Char -> isspace(c)))

skip(p::Parser, fq::Function)::Parser = and_then(p, _ -> fq())

between(open::Parser, close::Parser, fp::Function)::Parser =
    and_then(skip(open, fp), x -> skip(close, () -> mreturn(Parser, x)))

token(p::Parser)::Parser = between(spaces, spaces, () -> p)

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

end # module MonadParser
