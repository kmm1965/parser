from typing import Callable

from maybe import T
from parser import Parser, between
from some_parsers import identifier, symbol, double, usign

from math import sin, cos, asin, acos, sinh, cosh, asinh, acosh, tan, log, log10, exp, sqrt, pi, e

class Calculator(object):
    def __init__(self):
        pass

    def expr_in_brackets(self) -> Parser[float]:
        return between(symbol('('), symbol(')'), self.expr)
    
    def factor0(self) -> Parser[float]:
        return self.expr_in_brackets() | \
               self.funcs * self.expr_in_brackets | \
               self.consts | \
               double
    
    def factor(self) -> Parser[float]:
        return self.factor0().chainr1(self.fpow)
    
    def term(self) -> Parser[float]:
        return self.factor().chainl1(self.mul | self.div, False)
    
    def expr(self) -> Parser[float]:
        return usign.and_then(lambda sgn: self.term().chainl1(self.add | self.sub, sgn == "-"))
    
    def op2(c: str, f: Callable[[float, float], float]) -> Parser[Callable[[float, float], float]]:
        return symbol(c) >> (lambda: Parser.pure(f))
    
    add = op2('+', lambda x, y: x + y)
    sub = op2('-', lambda x, y: x - y)
    mul = op2('*', lambda x, y: x * y)
    div = op2('/', lambda x, y: x / y)
    fpow = op2('^', pow)

    def guard(b: bool, value: T) -> Parser[T]:
        return Parser.pure(value) if b else Parser.empty()

    funcs = identifier.and_then(lambda n:
        Calculator.guard(n == "sin",   sin)   |
        Calculator.guard(n == "cos",   cos)   |
        Calculator.guard(n == "asin",  asin)  |
        Calculator.guard(n == "acos",  acos)  |
        Calculator.guard(n == "sinh",  sinh)  |
        Calculator.guard(n == "cosh",  cosh)  |
        Calculator.guard(n == "asinh", asinh) |
        Calculator.guard(n == "acosh", acosh) |
        Calculator.guard(n == "tan",   tan)   |
        Calculator.guard(n == "log",   log)   |
        Calculator.guard(n == "log10", log10) |
        Calculator.guard(n == "exp",   exp)   |
        Calculator.guard(n == "sqrt",  sqrt)  |
        Calculator.guard(n == "sqr",   lambda x: x * x))

    consts = identifier.and_then(lambda n:
        Calculator.guard(n == "E",        e)          |
        Calculator.guard(n == "PI",       pi)         |
        Calculator.guard(n == "LOG2E",    1.44269504088896340736)  | # log2(e)
        Calculator.guard(n == "LOG10E",   0.434294481903251827651) | # log10(e)
        Calculator.guard(n == "LN2",      0.693147180559945309417) | # ln(2)
        Calculator.guard(n == "LN10",     2.30258509299404568402)  | # ln(10)
        Calculator.guard(n == "PI_2",     1.57079632679489661923)  | # pi/2
        Calculator.guard(n == "PI_4",     0.785398163397448309616) | # pi/4
        Calculator.guard(n == "1_PI",     0.318309886183790671538) | # 1/pi
        Calculator.guard(n == "2_PI",     0.636619772367581343076) | # 2/pi
        Calculator.guard(n == "2_SQRTPI", 1.12837916709551257390)  | # 2/sqrt(pi)
        Calculator.guard(n == "SQRT2",    1.41421356237309504880)  | # sqrt(2)
        Calculator.guard(n == "SQRT1_2",  0.707106781186547524401))  # 1/sqrt(2)

def calculate(inp):
    return Calculator().expr().parse(inp)
