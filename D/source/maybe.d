// Maybe support

import optional;

alias Maybe(T) = Optional!T;

Maybe!T Just(T)(T x) pure {
    return some(x);
}

Maybe!T Nothing(T)() pure {
    return no!T;
}

///
@("Just and Nothing unit tests")
@safe pure nothrow unittest {
    assert(Just(3) == some(3));
    assert(Nothing!int == no!int);
}

// Functor
auto transform(alias func, T)(Optional!T opt) pure
    if (is (typeof(func(T.init))))
{
    alias U = typeof(func(T.init));
    return !opt.empty ? Just(func(opt.front)) : Nothing!U;
}

///
@("Maybe transform unit tests")
@safe pure nothrow unittest {
    import std.math: sin;
    import std.conv: to;

    assert(Just(1.).transform!sin == (1.).sin.some);
    assert(Nothing!double.transform!sin == no!double);

    assert(Just(1).transform!(to!string) == some("1"));
    assert(Nothing!int.transform!(to!string) == no!string);
}

// Applicative
Maybe!T Maybe_pure(T)(T x) pure {
    return Just(x);
}

///
@("Example of Maybe_pure")
@safe pure nothrow unittest {
    assert(Maybe_pure(3) == some(3));
}

// Monad
auto and_then(alias func, T)(Optional!T opt) pure
    if (isOptional!(typeof(func(T.init))))
{
    alias U = OptionalTarget!(typeof(func(T.init)));
    return !opt.empty ? func(opt.front) : Nothing!U;
}

///
@("Maybe and_then unit tests")
@safe pure nothrow unittest {
    import std.math: sqrt, log;
    import std.conv: to;

    auto safe_sqrt = (double x) => x >= 0 ? x.sqrt.Just : Nothing!double;
    auto safe_log  = (double x) => x >  0 ? x.log .Just : Nothing!double;

    assert(safe_sqrt(2.) == (2.).sqrt.some);
    assert(safe_sqrt(2.).and_then!safe_log == (2.).sqrt.log.some);
    assert(safe_sqrt(-2.).and_then!safe_log == no!double);

    assert(Just(2.).and_then!safe_sqrt.and_then!safe_log == (2.).sqrt.log.some);
    assert(safe_sqrt(0. ).and_then!safe_log == no!double);
    assert(safe_sqrt(-2.).and_then!safe_log == no!double);

    assert(Nothing!double.and_then!safe_sqrt == no!double);
    assert(Nothing!double.and_then!safe_sqrt.and_then!safe_log == no!double);

    auto my_tostring = (int x) => x % 2 == 0 ? x.to!string.Just : Nothing!string;
    assert(Just(2).and_then!my_tostring == some("2"));
    assert(Just(1).and_then!my_tostring == no!string);
    assert(Nothing!int.and_then!my_tostring == no!string);
}
