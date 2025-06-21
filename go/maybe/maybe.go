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

func Map[A, B any](x Maybe[A], f func(A) B) Maybe[B] {
    if x.IsSome(){
        return Just(f(x.Get()))
    } else { return Nothing[B]() }
}

func FlatMap[A, B any](x Maybe[A], f func(A) Maybe[B]) Maybe[B] {
    if x.IsSome(){
        return f(x.Get())
    } else { return Nothing[B]() }
}

func OrElseGet[A any](x Maybe[A], f func() Maybe[A]) Maybe[A] {
    if x.IsSome(){
        return x
    } else { return f() }
}
