package maybe

import "gooptional"

type Maybe[A any] = gooptional.Optional[A]

func Nothing[A any]() Maybe[A] {
    return gooptional.None[A]()
}

func Just[A any](a A) Maybe[A] {
    return gooptional.Some[A](a)
}

func Pure[A any](a A) Maybe[A] {
    return Just(a)
}

func Map[A, B any](m Maybe[A], f func(A) B) Maybe[B] {
    return FlatMap(m, func (a A) Maybe[B] { return Just(f(a)) })
}

func FlatMap[A, B any](m Maybe[A], f func(A) Maybe[B]) Maybe[B] {
    if m.IsSome(){
        return f(m.Get())
    } else { return Nothing[B]() }
}

func OrElseGet[A any](x Maybe[A], f func() Maybe[A]) Maybe[A] {
    if x.IsSome(){
        return x
    } else { return f() }
}
