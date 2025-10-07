include("Parser.jl")

module SomeParsers

import Monads: (>>)

using ..MonadParser
import ..MonadParser: Parser, mreturn, and_then, |, +, satisfy, optional_s, optional_c, ~, many

export alnum, char, symbol, name, sign, usign, double

alnum::Parser = satisfy(c -> isletter(c) || isdigit(c) || c == '_')

char(x::Char)::Parser = satisfy(c::Char -> c == x)

symbol(c::Char)::Parser = ~char(c)

name(n::String)::Parser = ~and_then(+alnum, s -> s == n ? mreturn(Parser, n) : empty(Parser))

double1::Parser = ~and_then(+satisfy(c -> isdigit(c)), s -> mreturn(Parser, parse(Float64, s)))

digit::Parser = satisfy(c -> isdigit(c))

digits::Parser = many(digit)

sign::Parser = optional_c(char('+') | char('-'))

usign::Parser = optional_c(symbol('+') | symbol('-'))

double::Parser = ~and_then(digits,
    int_part  -> and_then(optional_s(char('.') >> digits),
    frac_part -> and_then(optional_s(and_then((char('e') | char('E')) >> sign,
        exp_sign   -> and_then(+digit,
        exp_digits -> mreturn(Parser, exp_sign * exp_digits)))),
    exp_part  -> length(int_part) > 0 || length(frac_part) > 0 ?
        mreturn(Parser, parse(Float64, int_part *
            (length(frac_part) > 0 ? '.' * frac_part : "") *
            (length(exp_part) > 0 ? 'e' * exp_part : ""))) :
        empty(Parser))))

end # module SomeParsers
