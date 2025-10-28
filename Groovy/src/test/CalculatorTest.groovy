package test

import main.Calculator

static test_funcs() {
    assert(Calculator.calc("sin(2.0)") == Optional.of(new Tuple2(Math.sin(2.0), "")))
    assert(Calculator.calc("cos(2.0)") == Optional.of(new Tuple2(Math.cos(2.0), "")))
    assert(Calculator.calc("asin(0.5)") == Optional.of(new Tuple2(Math.asin(0.5), "")))
    assert(Calculator.calc("acos(0.5)") == Optional.of(new Tuple2(Math.acos(0.5), "")))
    assert(Calculator.calc("sinh(2.0)") == Optional.of(new Tuple2(Math.sinh(2.0), "")))
    assert(Calculator.calc("cosh(2.0)") == Optional.of(new Tuple2(Math.cosh(2.0), "")))
    assert(Calculator.calc("tan(2.0)") == Optional.of(new Tuple2(Math.tan(2.0), "")))
    assert(Calculator.calc("log(2.0)") == Optional.of(new Tuple2(Math.log(2.0), "")))
    assert(Calculator.calc("log10(2.0)") == Optional.of(new Tuple2(Math.log10(2.0), "")))
    assert(Calculator.calc("exp(2.0)") == Optional.of(new Tuple2(Math.exp(2.0), "")))
    assert(Calculator.calc("sqrt(2.0)") == Optional.of(new Tuple2(Math.sqrt(2.0), "")))
    assert(Calculator.calc("sqr(2.0)") == Optional.of(new Tuple2(4.0, "")))
}

static test_constants() {
    assert(Calculator.calc("E") == Optional.of(new Tuple2(Math.E, "")))
    assert(Calculator.calc("LOG2E") == Optional.of(new Tuple2(1 / Math.log(2.0), "")))
    assert(Calculator.calc("LOG10E") == Optional.of(new Tuple2(0.434294481903251827651, "")))
    assert(Calculator.calc("LN2") == Optional.of(new Tuple2(Math.log(2.0), "")))
    assert(Calculator.calc("LN10") == Optional.of(new Tuple2(Math.log(10.0), "")))
    assert(Calculator.calc("PI") == Optional.of(new Tuple2(Math.PI, "")))
    assert(Calculator.calc("PI_2") == Optional.of(new Tuple2(Math.PI / 2, "")))
    assert(Calculator.calc("PI_4") == Optional.of(new Tuple2(Math.PI / 4, "")))
    assert(Calculator.calc("1_PI") == Optional.of(new Tuple2(1 / Math.PI, "")))
    assert(Calculator.calc("2_PI") == Optional.of(new Tuple2(2 / Math.PI, "")))
    assert(Calculator.calc("2_SQRTPI") == Optional.of(new Tuple2(2 / Math.sqrt(Math.PI), "")))
    assert(Calculator.calc("SQRT2") == Optional.of(new Tuple2(Math.sqrt(2.0), "")))
    assert(Calculator.calc("SQRT1_2") == Optional.of(new Tuple2(Math.sqrt(0.5), "")))
}


static test_calculator() {
    assert(Calculator.calc("72 - 7 - (1 - 2) * 3") == Optional.of(new Tuple2(72 - 7 - (1 - 2) * 3, "")))
    assert(Calculator.calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") == Optional.of(new Tuple2(7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3), "")))
    assert(Calculator.calc("3^(1+1)^3").map { pair -> new Tuple2(pair.getV1().round(), pair.getV2()) } == Optional.of(new Tuple2(6561.0, "")))
    assert(Calculator.calc("sin(1+1)") == Optional.of(new Tuple2(Math.sin(2.0), "")))
    assert(Calculator.calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )") == Optional.of(new Tuple2(Math.sin(2 / Math.sqrt(Math.PI) * 4 - 1), "")))
    assert(Calculator.calc("sqr(2 + 3)") == Optional.of(new Tuple2(25.0, "")))
    assert(Calculator.calc("sin(-PI/4)") == Optional.of(new Tuple2(Math.sin(-Math.PI/4), "")))
    assert(Calculator.calc(" E ^ PI") == Optional.of(new Tuple2(Math.pow(Math.E, Math.PI), "")))
    assert(Calculator.calc(" PI ^ E") == Optional.of(new Tuple2(Math.pow(Math.PI, Math.E), "")))
}

test_funcs()
test_constants()
test_calculator()
