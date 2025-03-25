include("Parser.jl")

module SomeParsers

import Monads: (>>)

using ..MonadParser
import ..MonadParser: Parser, mreturn, and_then, |, satisfy, optional_s, optional_c, token, some, many

export alnum, char, symbol, name, double

alnum::Parser = satisfy(c -> isletter(c) || isdigit(c) || c == '_')

char(x::Char)::Parser = satisfy(c::Char -> c == x)

symbol(c::Char)::Parser = token(char(c))

name(n::String)::Parser = token(and_then(some(alnum), s -> s == n ? mreturn(Parser, n) : empty(Parser)))

double1::Parser = token(and_then(some(satisfy(c -> isdigit(c))), s -> mreturn(Parser, parse(Float64, s))))

digits::Parser = many(satisfy(c -> isdigit(c)))

sign::Parser = optional_c(char('+') | char('-'))

double::Parser = token(and_then(sign,
    sign_part -> and_then(digits,
    int_part  -> and_then(optional_s(char('.') >> digits),
    frac_part -> and_then(optional_s(and_then((char('e') | char('E')) >> sign,
        exp_sign   -> and_then(some(satisfy(c -> isdigit(c))),
        exp_digits -> mreturn(Parser, exp_sign * exp_digits)))),
    exp_part  -> length(int_part) > 0 || length(frac_part) > 0 ?
        mreturn(Parser, parse(Float64, sign_part * int_part *
            (length(frac_part) > 0 ? '.' * frac_part : "") *
            (length(exp_part) > 0 ? 'e' * exp_part : ""))) :
        empty(Parser))))))

end # module SomeParsers
