from typing import TypeVar, Generic, Callable

from maybe import Maybe, T, U

Open = TypeVar('Open')    # Define type variable "Open"
Close = TypeVar('Close')  # Define type variable "Close"

def invoke(f):
    return f() if callable(f) else f

def between(open: "Parser[Open]", close: "Parser[Close]", p: "Parser[T]") -> "Parser[T]":
    return (open >> p).and_then(lambda e: close >> Parser.pure(e))

class Parser(Generic[T]):
    def __init__(self, unp: Callable[[str], tuple[T, str]]):
        self.unp = unp

    def parse(self, inp: str) -> tuple[T, str]:
        return self.unp(inp)
        
    # Functor
    def fmap(self, f: Callable[[T], U]) -> "Parser[U]":
        return Parser(lambda inp: self.parse(inp).fmap(lambda pair: (f(pair[0]), pair[1])))

    # Monad
    def and_then(self, f: Callable[[T], U]) -> "Parser[U]":
        return Parser(lambda inp: self.parse(inp).and_then(lambda pair: f(pair[0]).parse(pair[1])))

    # Applicative
    def pure(val: T) -> "Parser[T]":
        return Parser(lambda inp: Maybe.pure((val, inp)))

    def __mul__(self, p: "Callable[[], Parser[U]]") -> "Parser[U]":
        return self.and_then(lambda f: invoke(p).fmap(f))

    # Alternative
    def empty() -> Maybe[T]:
        return Parser(lambda inp: Maybe.empty())
        
    def __or__(self, els: "Parser[T]") -> "Parser[T]":
        return Parser(lambda inp: self.parse(inp) | invoke(els).parse(inp))
        
    def some(self) -> "Parser[str]":
        return self.fmap(lambda c: lambda s: c + s) * self.many

    def many(self) -> "Parser[str]":
        return self.some() | Parser.pure("")

    def __rshift__(self, p: "Parser[U]") -> "Parser[U]":
        return self.and_then(lambda _: invoke(p))

    def token(self) -> "Parser[T]":
        return between(spaces, spaces, self)

    def rest(fval: "Callable[[], Parser[T]]", ff: "Callable[[T], Parser[T]]", op: "Parser[Callable[[T, T], T]]", a: T) -> "Parser[T]":
        return op.and_then(lambda f: fval().and_then(lambda b: ff(f(a, b)))) | Parser.pure(a)

    def rest_l(self, op: "Parser[Callable[[T, T], T]]", a: T) -> "Parser[T]":
        return Parser.rest(lambda: self, lambda b: self.rest_l(op, b), op, a)
    
    def rest_r(self, op: "Parser[Callable[[T, T], T]]", a: T) -> "Parser[T]":
        return Parser.rest(lambda: self.chainr1(op), Parser.pure, op, a)
    
    def chainl1(self, op: "Parser[Callable[[T, T], T]]") -> "Parser[T]":
        return self.and_then(lambda a: self.rest_l(op, a))
    
    def chainr1(self, op: "Parser[Callable[[T, T], T]]") -> "Parser[T]":
        return self.and_then(lambda a: self.rest_r(op, a))
    
anyChar = Parser(lambda inp: Maybe.empty() if len(inp) == 0 else Maybe.pure((inp[0], inp[1:])))

def satisfy(f: Callable[[str], bool]) -> Parser[str]:
    return anyChar.and_then(lambda c: Parser.pure(c) if f(c) else Parser.empty())

spaces = satisfy(str.isspace).many()
