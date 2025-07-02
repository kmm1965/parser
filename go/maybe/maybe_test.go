package maybe_test

import (
	"testing"
	"math"
	"strconv"
	"maybe"
	"gooptional"
)

func TestMaybeMap(t *testing.T){
    if maybe.Map(maybe.Just(1.), math.Sin).Get() != math.Sin(1.) {
        t.Errorf("Just(1.).Map(Sin).Get() is not equal to Sin(1.)")
    }

    if maybe.Map(maybe.Nothing[float64](), math.Sin).IsSome() {
        t.Errorf("Nothing.Map(Sin).Get() is not empty")
    }

    if maybe.Map(maybe.Just(1), strconv.Itoa).Get() != "1" {
        t.Errorf("Just(1).Map(Itoa) is not equal to \"1\"")
    }

    if maybe.Map(maybe.Nothing[int](), strconv.Itoa).IsSome() {
        t.Errorf("Nothing.Map(Itoa) is not empty")
    }
}

func TestMaybePure(t *testing.T){
    if maybe.Pure(3) != gooptional.Some(3) {
        t.Errorf("Pure is not equal to Some")
    }
}

func TestMaybeFlatMap(t *testing.T){
    safe_sqrt := func (x float64) maybe.Maybe[float64] {
        if x >= 0 {
            return maybe.Just(math.Sqrt(x))
        } else { return maybe.Nothing[float64]() }
    }
    safe_log := func (x float64) maybe.Maybe[float64] {
        if x > 0 {
            return maybe.Just(math.Log(x))
        } else { return maybe.Nothing[float64]() }
    }

    if safe_sqrt(2.) != maybe.Just(math.Sqrt(2.)) {
        t.Errorf("safe_sqrt(2.) is not equal to Just(sqrt(2.))")
    }
    if safe_sqrt(-2.) != maybe.Nothing[float64]() {
        t.Errorf("safe_sqrt(-2.) is not equal to Nothing")
    }

    if safe_log(2.) != maybe.Just(math.Log(2.)) {
        t.Errorf("safe_log(2.) is not equal to Just(log(2.))")
    }
    if safe_log(-2.) != maybe.Nothing[float64]() {
        t.Errorf("safe_log(-2.) is not equal to Nothing")
    }

    if maybe.FlatMap(maybe.Just(2.), safe_sqrt) != maybe.Just(math.Sqrt(2.)) {
        t.Errorf("Just(2.).FlatMap(safe_sqrt) is not equal to Just(sqrt(2.))")
    }
    if maybe.FlatMap(maybe.Just(0.), safe_sqrt) != maybe.Just(0.) {
        t.Errorf("Just(2.).FlatMap(safe_sqrt) is not equal to Just(0.)")
    }
    if maybe.FlatMap(maybe.Nothing[float64](), safe_sqrt) != maybe.Nothing[float64]() {
        t.Errorf("Nothing.FlatMap(safe_sqrt) is not equal to Nothing")
    }
    if maybe.FlatMap(maybe.FlatMap(maybe.Just(2.), safe_sqrt), safe_log) != maybe.Just(math.Log(math.Sqrt(2.))) {
        t.Errorf("Just(2.).FlatMap(safe_sqrt).FlatMap(safe_log) is not equal to Just(log(sqrt(2.)))")
    }
    if maybe.FlatMap(maybe.FlatMap(maybe.Just(-2.), safe_sqrt), safe_log) != maybe.Nothing[float64]() {
        t.Errorf("Just(-2.).FlatMap(safe_sqrt).FlatMap(safe_log) is not equal to Nothing")
    }
    if maybe.FlatMap(maybe.FlatMap(maybe.Just(0.), safe_sqrt), safe_log) != maybe.Nothing[float64]() {
        t.Errorf("Just(0.).FlatMap(safe_sqrt).FlatMap(safe_log) is not equal to Nothing")
    }

    if maybe.FlatMap(safe_sqrt(2.), safe_log) != maybe.Just(math.Log(math.Sqrt(2.))) {
        t.Errorf("safe_sqrt(2.).FlatMap(safe_log) is not equal to Just(log(sqrt(2.)))")
    }
    if maybe.FlatMap(safe_sqrt(-2.), safe_log) != maybe.Nothing[float64]() {
        t.Errorf("safe_sqrt(-2.).FlatMap(safe_log) is not equal to Nothing")
    }
    if maybe.FlatMap(safe_sqrt(0.), safe_log) != maybe.Nothing[float64]() {
        t.Errorf("safe_sqrt(-2.).FlatMap(safe_log) is not equal to Nothing")
    }

    if maybe.FlatMap(maybe.Nothing[float64](), safe_sqrt) != maybe.Nothing[float64]() {
        t.Errorf("Nothing.FlatMap(safe_sqrt) is not equal to Nothing")
    }

    to_string := func(x int) maybe.Maybe[string] {
        if x % 2 == 0 {
            return maybe.Just(strconv.Itoa(x))
        } else { return maybe.Nothing[string]() }
    }

    if maybe.FlatMap(maybe.Just(2), to_string) != maybe.Just("2") {
        t.Errorf("Just(2).FlatMap(to_string) is not equal to Just(\"2\")")
    }
    if maybe.FlatMap(maybe.Just(1), to_string) != maybe.Nothing[string]() {
        t.Errorf("Just(1).FlatMap(to_string) is not equal to Nothing")
    }
    if maybe.FlatMap(maybe.Nothing[int](), to_string) != maybe.Nothing[string]() {
        t.Errorf("Nothing.FlatMap(to_string) is not equal to Nothing")
    }
}
