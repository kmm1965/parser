package parser

import (
    "pair"
    "maybe"
)

type ParserPair[A any] = pair.KeyValue[A, string]

type ParserResult[A any] = maybe.Maybe[ParserPair[A]]

type Parser[A any] struct {
    Parse func(string) ParserResult[A]
}

// Functor
func Map[A, B any](p Parser[A], f func(A) B) Parser[B] {
    return FlatMap(p, func (a A) Parser[B] { return Pure(f(a)) })
}

// Applicative
func Pure[A any](a A) Parser[A] {
    return Parser[A]{ func (inp string) ParserResult[A] {
        return maybe.Just(ParserPair[A]{ a, inp })
    }}
}

func Apply[A, B any](pf Parser[func(A) B], fp func() Parser[A]) Parser[B] {
    return FlatMap(pf, func (f func(A) B) Parser[B] {
        return Map(fp(), f)
    })
}

//Monad
func FlatMap[A, B any](p Parser[A], f func(A) Parser[B]) Parser[B] {
    return Parser[B]{ func (inp string) ParserResult[B] {
        return maybe.FlatMap(p.Parse(inp), func (pair ParserPair[A]) ParserResult[B] {
            return f(pair.Key).Parse(pair.Value)
        })
    }}
}

func SkipGet[A, B any](p Parser[A], fq func () Parser[B]) Parser[B] {
    return FlatMap(p, func (_ A) Parser[B] { return fq() })
}

func Skip[A, B any](p Parser[A], q Parser[B]) Parser[B] {
    return SkipGet(p, func () Parser[B] { return q })
}

// Alternative
func Empty[A any]() Parser[A] {
    return Parser[A]{ func (inp string) ParserResult[A] {
        return maybe.Nothing[ParserPair[A]]()
    }}
}

func OrElseGet[A any](p Parser[A], f func() Parser[A]) Parser[A] {
    return Parser[A]{ func (inp string) ParserResult[A] {
        return maybe.OrElseGet(p.Parse(inp), func() ParserResult[A] {
            return f().Parse(inp)
        })
    }}
}

func OrElse[A any](p Parser[A], q Parser[A]) Parser[A] {
    return OrElseGet(p, func () Parser[A] { return q })
}
