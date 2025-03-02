from typing import Callable

from parser import Parser, symbol, between, natural, name
from maybe import T

from math import sin, cos, acos, sinh, cosh, asinh, acosh, tan, log, log10, exp, sqrt, pi, e

class Calculator(object):
    def __init__(self):
        pass

    br_open  = symbol('(')
    br_close = symbol(')')

    def expr_in_brackets(self) -> Parser[float]:
        return between(self.br_open, self.br_close, self.expr)
    
    def factor0(self) -> Parser[float]:
        return self.expr_in_brackets() | self.func * self.expr_in_brackets | self._const | natural
    
    def factor(self) -> Parser[float]:
        return self.factor0().chainr1(self.fpow)
    
    def term(self) -> Parser[float]:
        return self.factor().chainl1(self.mul | self.div)
    
    def expr(self) -> Parser[float]:
        return self.term().chainl1(self.add | self.sub)
    
    def op2(c: str, f: Callable[[float, float], float]) -> Parser[Callable[[float, float], float]]:
        return symbol(c).skip(lambda: Parser.pure(f))
    
    add = op2('+', lambda x, y: x + y)
    sub = op2('-', lambda x, y: x - y)
    mul = op2('*', lambda x, y: x * y)
    div = op2('/', lambda x, y: x / y)
    fpow = op2('^', pow)

    def fold(parsers: list[Parser[T]]) -> Parser[T]:
        p0 = Parser.empty()
        for p in parsers:
            p0 |= p
        return p0

    def def_object(n: str, value: T) -> Parser[T]:
        return name(n).skip(lambda: Parser.pure(value))
    
    def sqr(x):
        return x * x

    functions = [
        def_object("sin",   sin),
        def_object("cos",   cos),
        def_object("acos",  acos),
        def_object("sinh",  sinh),
        def_object("cosh",  cosh),
        def_object("asinh", asinh),
        def_object("acosh", acosh),
        def_object("tan",   tan),
        def_object("log",   log),
        def_object("log10", log10),
        def_object("exp",   exp),
        def_object("sqrt",  sqrt),
        def_object("sqr",   sqr)
    ]

    func = fold(functions).token()

    M_LOG2E    = 1.44269504088896340736;  # log2(e)
    M_LOG10E   = 0.434294481903251827651; # log10(e)
    M_LN2      = 0.693147180559945309417; # ln(2)
    M_LN10     = 2.30258509299404568402;  # ln(10)
    M_PI_2     = 1.57079632679489661923;  # pi/2
    M_PI_4     = 0.785398163397448309616; # pi/4
    M_1_PI     = 0.318309886183790671538; # 1/pi
    M_2_PI     = 0.636619772367581343076; # 2/pi
    M_2_SQRTPI = 1.12837916709551257390;  # 2/sqrt(pi)
    M_SQRT2    = 1.41421356237309504880;  # sqrt(2)
    M_SQRT1_2  = 0.707106781186547524401; # 1/sqrt(2)

    constants = [
        def_object("E",        e),
        def_object("PI",       pi),
        def_object("LOG2E",    M_LOG2E),
        def_object("LOG10E",   M_LOG10E),
        def_object("LN2",      M_LN2),
        def_object("LN10",     M_LN10),
        def_object("PI_2",     M_PI_2),
        def_object("PI_4",     M_PI_4),
        def_object("1_PI",     M_1_PI),
        def_object("2_PI",     M_2_PI),
        def_object("2_SQRTPI", M_2_SQRTPI),
        def_object("SQRT2",    M_SQRT2),
        def_object("SQRT1_2",  M_SQRT1_2)
    ]

    _const = fold(constants).token();
