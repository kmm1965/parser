from typing import TypeVar, Generic, Callable

T = TypeVar('T')  # Define type variable "T"
U = TypeVar('U')  # Define type variable "U"

class Maybe(Generic[T]):
    def __init__(self, value: T | None = None):
        self.value = value

    def empty() ->"Maybe[T]":
        return Maybe()
        
    def pure(val: T) -> "Maybe[T]":
        return Maybe(val)

    def fmap(self, f: Callable[[T], U]) -> "Maybe[U]":
        return self if self.value is None else Maybe.pure(f(self.value))

    def and_then(self, f: Callable[[T], U]) -> "Maybe[U]":
        return self if self.value is None else f(self.value)

    def or_else(self, els) -> "Maybe[T]":
        return self if not self.value is None else els() if callable(els) else els

    def __or__(self, els) -> "Maybe[T]":
        return self.or_else(els)
        
    # def __mul__(self, m: Callable[[], Maybe[U]]) -> "Maybe[U]":
    def __mul__(self, m) -> "Maybe[U]":
        return self.and_then(lambda f: (m() if callable(m) else m).fmap(f))
