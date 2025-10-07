import unittest

from maybe import Maybe
from parser import Parser, anyChar, satisfy, spaces, between
from some_parsers import char, optional_s, symbol, alnum, name, sign, usign, digits, double, symbol
from calculator import calculate
from math import sin, sqrt, log, exp

def safe_sqrt(x):
    return Maybe.pure(sqrt(x)) if x >= 0 else Maybe.empty()
    
def safe_log(x):
    return Maybe.pure(log(x)) if x > 0 else Maybe.empty()

def to_string(x):
    return  Maybe.pure(str(x)) if x % 2 == 0 else Maybe.empty()

class TestParser(unittest.TestCase):
    def setUp(self):
        self.mempty = Maybe.empty()
        self.pempty = Parser.empty()
        #self.calc = Calculator()
    
    def test_maybe_fmap(self):
        toString = lambda x: str(x)
        self.assertEqual(Maybe.pure(1.0).fmap(sin).value, sin(1.0))
        self.assertEqual(self.mempty.fmap(sin).value, None)
        self.assertEqual(Maybe.pure(1).fmap(toString).value, "1")
        self.assertEqual(self.mempty.fmap(toString).value, None)

    def test_maybe_flat_map(self):
        self.assertEqual(safe_sqrt(2.0).value, sqrt(2.0))
        self.assertEqual(safe_sqrt(0.0).value, sqrt(0.0))
        self.assertEqual(safe_sqrt(-2.0).value, None)

        self.assertEqual(safe_log(2.0).value, log(2.0))
        self.assertEqual(safe_log(0.0).value, None)
        self.assertEqual(safe_log(-2.0).value, None)

        self.assertEqual(Maybe.pure(2.0).and_then(safe_sqrt).value, sqrt(2.0))
        self.assertEqual(Maybe.pure(0.0).and_then(safe_sqrt).value, sqrt(0.0))
        self.assertEqual(self.mempty.and_then(safe_sqrt).value, None)

        self.assertEqual(Maybe.pure(2.0).and_then(safe_sqrt).and_then(safe_log).value, log(sqrt(2.0)))
        self.assertEqual(Maybe.pure(-2.0).and_then(safe_sqrt).and_then(safe_log).value, None)
        self.assertEqual(Maybe.pure(0.0).and_then(safe_sqrt).and_then(safe_log).value, None)

        self.assertEqual(safe_sqrt(2.0).and_then(safe_log).value, log(sqrt(2.0)))
        self.assertEqual(safe_sqrt(-2.0).and_then(safe_log).value, None)
        self.assertEqual(safe_sqrt(0.0).and_then(safe_log).value, None)
        self.assertEqual(self.mempty.and_then(safe_log).value, None)

        self.assertEqual(Maybe.pure(2).and_then(to_string).value, "2")
        self.assertEqual(Maybe.pure(1).and_then(to_string).value, None)
        self.assertEqual(self.mempty.and_then(to_string).value, None)

    def test_parser_pure(self):
        self.assertEqual(Parser.pure(1).parse("abc").value, (1, "abc"))
        self.assertEqual(Parser.pure(1.0).parse("abc").value, (1.0, "abc"))
        self.assertEqual(Parser.pure("1").parse("abc").value, ("1", "abc"))

    def test_parser_functor(self):
        fx = lambda x: str(x)
        fs = lambda s: int(s)

        self.assertEqual(Parser.pure(1).fmap(fx).parse("abc").value, ("1", "abc"))
        self.assertEqual(Parser.pure(1.0).fmap(fx).parse("abc").value, ("1.0", "abc"))
        self.assertEqual(Parser.pure("1").fmap(fs).parse("abc").value, (1, "abc"))

        self.assertEqual(self.pempty.fmap(fx).parse("abc").value, None)
        self.assertEqual(self.pempty.fmap(fs).parse("abc").value, None)

    def test_parser_applicative(self):
        psin = Parser.pure(sin)
        fd = Parser.pure(1.0)

        self.assertEqual((psin * fd).parse("abc").value, (sin(1.0), "abc"))
        self.assertEqual((psin * self.pempty).parse("abc").value, None)
        self.assertEqual((self.pempty * fd).parse("abc").value, None)
        self.assertEqual((self.pempty * self.pempty).parse("abc").value, None)

    def test_parser_monad(self):
        i1 = Parser.pure(1)
        eat = lambda x: Parser(lambda inp: Maybe.pure((str(x) + inp, "")))
        cancel = lambda _: Parser(lambda _: self.mempty)

        self.assertEqual(i1.and_then(eat).parse("abc").value, ("1abc", ""))
        self.assertEqual(i1.and_then(cancel).parse("abc").value, None)
        self.assertEqual(self.pempty.and_then(eat).parse("abc").value, None)
        self.assertEqual(i1.and_then(cancel).parse("abc").value, None)

    def test_anyChar(self):
        self.assertEqual(anyChar.parse("abc").value, ('a', "bc"))
        self.assertEqual(anyChar.parse("").value, None)

    def test_satisfy(self):
        self.assertEqual(satisfy(lambda c: c == 'a').parse("abc").value, ('a', "bc"))
        self.assertEqual(satisfy(lambda c: c == 'z').parse("abc").value, None)

    def test_char(self):
        self.assertEqual(char('a').parse("abc").value, (('a', "bc")))
        self.assertEqual(char('z').parse("abc").value, None)

    def test_empty_string(self):
        self.assertEqual(Parser.pure("").parse("abc").value, ("", "abc"))

    def test_optional(self):
        self.assertEqual(optional_s(char('1')).parse("1234").value, ("1", "234"))
        self.assertEqual(optional_s(char('1')).parse("abc").value, ("", "abc"))

    def test_spaces(self):
        self.assertEqual(spaces.parse("abc").value, ("", "abc"))
        self.assertEqual(spaces.parse("  abc").value, ("  ", "abc"))

    def test_symbol(self):
        self.assertEqual(symbol('+').parse(" + abc").value, ('+', "abc"))
        self.assertEqual(symbol('+').parse("abc").value, None)

    def test_alnum(self):
        self.assertEqual(alnum.parse("123abc").value, ('1', "23abc"))
        self.assertEqual(alnum.parse("_123abc").value, ('_', "123abc"))
        self.assertEqual(alnum.parse("!@#$").value, None)

    def test_name(self):
        psin = name("sin")

        self.assertEqual(psin.parse(" sin ").value, ("sin", ""))
        self.assertEqual(psin.parse(" sin (1.)").value, ("sin", "(1.)"))
        self.assertEqual(psin.parse("sinabc").value, None)

    def test_sign(self):
        self.assertEqual(sign.parse("abc").value, ("", "abc"))
        self.assertEqual(sign.parse("+abc").value, ("+", "abc"))
        self.assertEqual(sign.parse("-abc").value, ("-", "abc"))

        self.assertEqual(usign.parse("abc").value, ("", "abc"))
        self.assertEqual(usign.parse(" + abc").value, ("+", "abc"))
        self.assertEqual(usign.parse(" - abc").value, ("-", "abc"))

    def test_digits(self):
        self.assertEqual(digits.parse("123abc").value, ("123", "abc"))
        self.assertEqual(digits.parse("123   abc").value, ("123", "   abc"))
        self.assertEqual(digits.parse("abc").value, ("", "abc"))

    def test_double(self):
        self.assertEqual(double.parse("1 abc").value, (1.0, "abc"))
        self.assertEqual(double.parse("1. abc").value, (1.0, "abc"))
        self.assertEqual(double.parse("1.23 abc").value, (1.23, "abc"))
        self.assertEqual(double.parse("-1.23 abc").value, None) #(-1.23, "abc"))
        self.assertEqual(double.parse(".23 abc").value, (0.23, "abc"))
        self.assertEqual(double.parse(" + 1.23 abc").value, None)
        self.assertEqual(double.parse("1.23e10abc").value, (1.23e10, "abc"))
        self.assertEqual(double.parse("1.23e-10abc").value, (1.23e-10, "abc"))
        self.assertEqual(double.parse("abc").value, None)

    def test_between(self):
        expr = between(symbol('('), symbol(')'), double)

        self.assertEqual(expr.parse(" ( 123 ) abc").value, (123.0, "abc"))
        self.assertEqual(expr.parse(" ( 123 abc").value, None)
        self.assertEqual(expr.parse(" 123 ) abc").value, None)

    def test_chainlr1(self):
        add = symbol('+') >> Parser.pure(lambda x, y: x + y)
        sub = symbol('-') >> Parser.pure(lambda x, y: x - y)
        pexpr = double.chainl1(add | sub, False)
        pow = symbol('^') >> Parser.pure(lambda x, y: exp(y * log(x)))

        self.assertEqual(pexpr.parse("7abc").value, (7.0, "abc"))
        self.assertEqual(pexpr.parse(" 7 - 1 - 2 abc").value, (4.0, "abc"))
        self.assertEqual(pexpr.parse(" 7 - 1 + 2 - 3 abc").value, (5.0, "abc"))
        self.assertEqual(pexpr.parse("abc").value, None)

        self.assertEqual(double.chainr1(pow).parse("3 ^ 2 ^ 3 abc").fmap(lambda p: (round(p[0]), p[1])).value, (6561.0, "abc"))

    def test_calculator(self):
        self.assertEqual(calculate("72 - 7 - (1 - 2) * 3").value, (68.0, ""))
        self.assertEqual(calculate("-72 - 7 - (1 - 2) * 3").value, (-76.0, ""))
        self.assertEqual(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").value, (-8.889, ""))
        self.assertEqual(calculate("3^(1+1)^3").fmap(lambda p: (round(p[0]), p[1])).value, (6561.0, ""))
        self.assertEqual(calculate("sin(1+1)").value, (sin(2.0), ""))
        self.assertEqual(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").value, (-0.3634085731426532, ""))
        self.assertEqual(calculate("sqr(2 + 3)").value, (25.0, ""))
        self.assertEqual(calculate("sin(-PI/4)").value, (-0.7071067811865476, ""))
        self.assertEqual(calculate(" E ^ PI").value, (23.140692632779263, ""))
        self.assertEqual(calculate(" PI ^ E").value, (22.45915771836104, ""))
        self.assertEqual(calculate("sqr(sin(2)) + sqr(cos(1 + 1))").value, (1, ""))

if __name__ == '__main__':
    unittest.main()