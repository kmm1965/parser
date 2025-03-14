from typing import TypeVar, Generic, Callable

from maybe import Maybe, T, U

class Parser(Generic[T]):
    def __init__(self, unp: Callable[[str], tuple[T, str]]):
        self.unp = unp

    def parse(self, inp: str) -> tuple[T, str]:
        return self.unp(inp)
        
    # Functor
    def fmap(self, f: Callable[[T], U]) -> "Parser[U]":
        return Parser(lambda inp: self.parse(inp).fmap(lambda pair: (f(pair[0]), pair[1])))

    # Monad
    # def and_then(self, f: Callable[[T], Parser[U]]) -> "Parser[U]":
    def and_then(self, f: Callable[[T], U]) -> "Parser[U]":
        return Parser(lambda inp: self.parse(inp).and_then(lambda pair: f(pair[0]).parse(pair[1])))

    def skip(self, p) -> "Parser[U]":
        return self.and_then(lambda _: p() if callable(p) else p)

    def __rshift__(self, p) -> "Parser[U]":
        return self.skip(p)

    # Applicative
    def pure(val: T) -> "Parser[T]":
        return Parser(lambda inp: Maybe.pure((val, inp)))

    # def __mul__(self, p: Callable[[], Parser[U]]) -> "Parser[U]":
    def __mul__(self, p) -> "Parser[U]":
        return self.and_then(lambda f: (p() if callable(p) else p).fmap(f))

    # Alternative
    def empty() -> Maybe[T]:
        return Parser(lambda inp: Maybe.empty())
        
    def or_else(self, els) -> "Parser[T]":
        return Parser(lambda inp: self.parse(inp) | (els() if callable(els) else els).parse(inp))

    def __or__(self, els) -> "Parser[T]":
        return self.or_else(els)
        
    def some(self) -> "Parser[str]":
        return self.fmap(lambda c: lambda s: c + s) * self.many

    def many(self) -> "Parser[str]":
        return self.some() | Parser.pure("")

    def token(self) -> "Parser[T]":
        return (spaces >> self).and_then(lambda a: spaces >> Parser.pure(a))

    # def rest(a: T, op: Parser[Callable[[T, T], T]], fval: [Callable[[], Parser[T]]], ff: Callable[[T], Parser[T]]) -> "Parser[T]:
    def rest(fval, ff, op, a: T) -> "Parser[T]":
        return op.and_then(lambda f: fval().and_then(lambda b: ff(f(a, b)))) | Parser.pure(a)

    # def rest_l(self, a: T, op: Parser[Callable[[T, T], T]]) -> "Parser[T]":
    def rest_l(self, op, a: T) -> "Parser[T]":
        return Parser.rest(lambda: self, lambda b: self.rest_l(op, b), op, a)
    
    # def rest_r(self, a: T, op: Parser[Callable[[T, T], T]]) -> "Parser[T]":
    def rest_r(self, op, a: T) -> "Parser[T]":
        return Parser.rest(lambda: self.chainr1(op), Parser.pure, op, a)
    
    # def chainl1(self, op: Parser[Callable[[T, T], T]]) -> "Parser[T]":
    def chainl1(self, op) -> "Parser[T]":
        return self.and_then(lambda a: self.rest_l(op, a))
    
    # def chainr1(self, op: Parser[Callable[[T, T], T]]) -> "Parser[T]":
    def chainr1(self, op) -> "Parser[T]":
        return self.and_then(lambda a: self.rest_r(op, a))
    
anyChar = Parser(lambda inp: Maybe.empty() if len(inp) == 0 else Maybe.pure((inp[0], inp[1:])))

def satisfy(f: Callable[[str], bool]) -> Parser[str]:
    return anyChar.and_then(lambda c: Parser.pure(c) if f(c) else Parser.empty())

alnum = satisfy(lambda c: c.isalnum() or c == '_')

spaces = satisfy(str.isspace).many()

def symbol(c: str) -> Parser[str]:
    return satisfy(lambda x: x == c).token()

def name(n: str) -> Parser[str]:
    return alnum.some().and_then(lambda s: Parser.pure(n).token() if s == n else Parser.empty())

natural = satisfy(str.isdigit).some().and_then(lambda s: Parser.pure(float(s)).token())

Open = TypeVar('Open')    # Define type variable "Open"
Close = TypeVar('Close')  # Define type variable "Close"

def between(open: Parser[Open], close: Parser[Close], fp: Callable[[], Parser[T]]) -> Parser[T]:
    return (open >> fp).and_then(lambda e: close >> Parser.pure(e))
