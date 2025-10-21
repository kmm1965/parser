package some_parsers

import (
    "parser"
    "maybe"
    "unicode"
    "strconv"
    "fmt"
)

func AnyChar() parser.Parser[byte] {
    return parser.Parser[byte] { func (inp string) parser.ParserResult[byte] {
        if inp == "" {
            return maybe.Nothing[parser.ParserPair[byte]]()
        } else {
            return maybe.Just(parser.ParserPair[byte]{ inp[0], inp[1:] })
        }
    }}
}

func Satisfy(pred func(byte) bool) parser.Parser[byte] {
    return parser.FlatMap(AnyChar(), func (c byte) parser.Parser[byte] {
        if pred(c) {
            return parser.Pure(c)
        } else { return parser.Empty[byte]() }
    })
}

func Char(c byte) parser.Parser[byte] {
    return Satisfy(func (x byte) bool { return x == c })
}

func EmptyString() parser.Parser[string] {
    return parser.Pure("")
}

func Optional_s(p parser.Parser[string]) parser.Parser[string] {
    return parser.OrElseGet(p, EmptyString)
}

func Optional_c(p parser.Parser[byte]) parser.Parser[string] {
    //return Optional_s(parser.Map(p, func(c byte) string { return string([]byte{c})}))
    return Optional_s(parser.Map(p, func(c byte) string { return string(c)}))
}

func Some(p parser.Parser[byte]) parser.Parser[string] {
    return parser.Apply(parser.Map(p, func (c byte) func(string) string {
            return func(s string) string { return string(c) + s }
        }),
        func () parser.Parser[string] { return Many(p) })
}

func Many(p parser.Parser[byte]) parser.Parser[string] {
    return parser.OrElseGet(Some(p), func () parser.Parser[string] { return parser.Pure("") })
}

func Spaces() parser.Parser[string] {
    return Many(Satisfy(func (c byte) bool { return unicode.IsSpace(rune(c)) }))
}

func BetweenGet[Open, Close, A any](open parser.Parser[Open], close parser.Parser[Close],
        fp func () parser.Parser[A]) parser.Parser[A] {
    return parser.FlatMap(parser.SkipGet(open, fp), func (a A) parser.Parser[A] {
        return parser.SkipGet(close, func () parser.Parser[A] { return parser.Pure(a) })
    })
}

func Between[Open, Close, A any](open parser.Parser[Open], close parser.Parser[Close],
        p parser.Parser[A]) parser.Parser[A] {
    return BetweenGet(open, close, func () parser.Parser[A] { return p })
}

func Token[A any](p parser.Parser[A]) parser.Parser[A] {
    spaces := Spaces()
    return Between(spaces, spaces, p)
}

func Symbol(c byte) parser.Parser[byte] {
    return Token(Char(c))
}

func Alnum() parser.Parser[byte] {
    return Satisfy(func (c byte) bool { return unicode.IsLetter(rune(c)) || unicode.IsDigit(rune(c)) || c == '_' })
}

func Name(n string) parser.Parser[string] {
    return parser.FlatMap(Token(Some(Alnum())), func (s string) parser.Parser[string] {
        if s == n {
            return parser.Pure(n)
        } else { return parser.Empty[string]() }
    })
}

func Sign() parser.Parser[string] {
    return Optional_c(parser.OrElse(Char('+'), Char('-')))
}

// Unary sign
func Usign() parser.Parser[string] {
    return Token(Sign())
}

func Digit() parser.Parser[byte] {
    return Satisfy(func (c byte) bool { return unicode.IsDigit(rune(c)) })
}

func Digits() parser.Parser[string] {
    return Many(Digit())
}

func iff[A any](cond bool, if_true A, if_false A) A {
    if cond { return if_true } else { return if_false }
}

func Double() parser.Parser[float64] {
    digits := Digits()
    sign := Sign()
    return Token(parser.FlatMap(sign,
        func (sign_part string) parser.Parser[float64] { return parser.FlatMap(digits,
        func (int_part string)  parser.Parser[float64] { return parser.FlatMap(Optional_s(parser.Skip(Char('.'), digits)),
        func (frac_part string) parser.Parser[float64] { return parser.FlatMap(Optional_s(parser.FlatMap(
                parser.Skip(parser.OrElse(Char('e'), Char('E')), sign),
            func (exp_sign string) parser.Parser[string] {
                return parser.FlatMap(Some(Digit()),
            func (exp_digits string) parser.Parser[string] {
                return parser.Pure(exp_sign + exp_digits) }) })),
        func (exp_part string) parser.Parser[float64] {
            if len(int_part) > 0 || len(frac_part) > 0 {
                sflt := int_part +
                    iff(len(frac_part) > 0, "." + frac_part, "") +
                    iff(len(exp_part) > 0, "e" + exp_part, "")
                flt, err := strconv.ParseFloat(sflt, 64)
                if err != nil {
                    fmt.Println("Error parsing a number:", sflt)
                    return parser.Empty[float64]()
                } else { return parser.Pure(flt) }
            } else { return parser.Empty[float64]() }
        }) }) }) }))
}

func Rest[A any] (fval func () parser.Parser[A], ff func(A) parser.Parser[A], op parser.Parser[func(A, A) A], x A) parser.Parser[A] {
    return parser.OrElseGet(
        parser.FlatMap(op, func (f func(A, A) A) parser.Parser[A] {
            return parser.FlatMap(fval(), func (y A) parser.Parser[A] { return ff(f(x, y)) })
        }), func () parser.Parser[A] { return parser.Pure(x) })
}

func Rest_l[A any] (p parser.Parser[A], op parser.Parser[func(A, A) A], x A) parser.Parser[A] {
    return Rest(func () parser.Parser[A] { return p },
        func (y A) parser.Parser[A] { return Rest_l(p, op, y) }, op, x)
}

func Rest_r[A any] (p parser.Parser[A], op parser.Parser[func(A, A) A], x A) parser.Parser[A] {
    return Rest(func () parser.Parser[A] { return Chainr1(p, op) }, parser.Pure[A], op, x)
}

func Chainl1(p parser.Parser[float64], op parser.Parser[func(float64, float64) float64], negate_first bool) parser.Parser[float64] {
    return parser.FlatMap(p, func (a float64) parser.Parser[float64] {
        return Rest_l(p, op, iff(negate_first, -a, a))
    } )
}

func Chainr1[A any] (p parser.Parser[A], op parser.Parser[func(A, A) A]) parser.Parser[A] {
    return parser.FlatMap(p, func (a A) parser.Parser[A] { return Rest_r(p, op, a) })
}
