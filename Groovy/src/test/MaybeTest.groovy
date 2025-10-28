package test

static Optional safe_sqrt(Double x) {
    return x >= 0 ? Optional.of(Math.sqrt(x)) : Optional.empty()
}

static Optional safe_log(Double x) {
    return x > 0 ? Optional.of(Math.log(x)) : Optional.empty()
}

static test_maybe_map() {
    assert(Optional.of(1.0).map { Double x -> Math.sin(x) } == Optional.of(Math.sin(1.0)))
    assert(Optional.empty().map { Double x -> Math.sin(x) } == Optional.empty())
    assert(Optional.of(1).map { i -> i.toString() } == Optional.of("1"))
    assert(Optional.empty().map { i -> i.toString() } ==  Optional.empty())
}

static test_maybe_flatMap() {
    assert(safe_sqrt(2.0) == Optional.of(Math.sqrt(2.0)))
    assert(safe_sqrt(0.0) == Optional.of(Math.sqrt(0.0)))
    assert(safe_sqrt(-2.0) == Optional.empty())

    assert(safe_log(2.0) == Optional.of(Math.log(2.0)))
    assert(safe_log(0.0) == Optional.empty())
    assert(safe_log(-2.0) == Optional.empty())

    assert(Optional.of(2.0).flatMap  { Double x -> safe_sqrt(x) } == Optional.of(Math.sqrt(2.0)))
    assert(Optional.of(0.0).flatMap  { Double x -> safe_sqrt(x) } == Optional.of(Math.sqrt(0.0)))
    assert(Optional.of(-2.0).flatMap { Double x -> safe_sqrt(x) } == Optional.empty())

    assert(Optional.of(2.0).flatMap  { Double x -> safe_sqrt(x) }.flatMap { Double x -> safe_log(x) } == Optional.of(Math.log(Math.sqrt(2.0))))
    assert(Optional.of(0.0).flatMap  { Double x -> safe_sqrt(x) }.flatMap { Double x -> safe_log(x) } == Optional.empty())
    assert(Optional.of(-2.0).flatMap { Double x -> safe_sqrt(x) }.flatMap { Double x -> safe_log(x) } == Optional.empty())

    assert(safe_sqrt(2.0).flatMap { Double x -> safe_log(x) } == Optional.of(Math.log(Math.sqrt(2.0))))
    assert(safe_sqrt(0.0).flatMap { Double x -> safe_log(x) } == Optional.empty())
    assert(safe_sqrt(-2.0).flatMap { Double x -> safe_log(x) } == Optional.empty())
    assert(Optional.empty().flatMap { Double x -> safe_log(x) } == Optional.empty())

    Closure toString = { i -> i % 2 == 0 ? Optional.of(i.toString()) :  Optional.empty() }

    assert(Optional.of(2).flatMap(toString) == Optional.of("2"))
    assert(Optional.of(1).flatMap(toString) == Optional.empty())
    assert(Optional.empty().flatMap(toString) == Optional.empty())
}

test_maybe_map()
test_maybe_flatMap()