local calc = require("../src/calculator")

function test(value, expected)
  print(expected == nil and value == expected or value ~= nil and value[1] == expected[1] and value[2] == expected[2])
end

function test_funcs()
  test(calc.calculate("sin(2.0)").value, {math.sin(2.0), ""})
  test(calc.calculate("cos(2.0)").value, {math.cos(2.0), ""})
  test(calc.calculate("asin(0.5)").value, {math.asin(0.5), ""})
  test(calc.calculate("acos(0.5)").value, {math.acos(0.5), ""})
  test(calc.calculate("sinh(2.0)").value, {math.sinh(2.0), ""})
  test(calc.calculate("cosh(2.0)").value, {math.cosh(2.0), ""})
  test(calc.calculate("tan(2.0)").value, {math.tan(2.0), ""})
  test(calc.calculate("log(2.0)").value, {math.log(2.0), ""})
  test(calc.calculate("log10(2.0)").value, {math.log10(2.0), ""})
  test(calc.calculate("exp(2.0)").value, {math.exp(2.0), ""})
  test(calc.calculate("sqrt(2.0)").value, {math.sqrt(2.0), ""})
  test(calc.calculate("sqr(2.0)").value, {4.0, ""})
end

function test_consts()
  test(calc.calculate("E").value, {2.7182818284590452, ""})
  test(calc.calculate("LOG2E").value, {1 / math.log(2.0), ""})
  test(calc.calculate("LOG10E").value, {0.4342944819032518, ""})
  --test(calc.calculate("LOG10E").value, {1 / math.log(10.0), ""})
  test(calc.calculate("LN2").value, {math.log(2.0), ""})
  test(calc.calculate("LN10").value, {math.log(10.0), ""})
  test(calc.calculate("PI").value, {math.pi, ""})
  test(calc.calculate("PI_2").value, {math.pi / 2, ""})
  test(calc.calculate("PI_4").value, {math.pi / 4, ""})
  test(calc.calculate("1_PI").value, {1 / math.pi, ""})
  test(calc.calculate("2_PI").value, {2 / math.pi, ""})
  test(calc.calculate("2_SQRTPI").value, {2 / math.sqrt(math.pi), ""})
  test(calc.calculate("SQRT2").value, {math.sqrt(2), ""})
  test(calc.calculate("SQRT1_2").value, {math.sqrt(0.5), ""})
end

function testCalculator()
  test(calc.calculate("72 - 7 - (1 - 2) * 3").value, {68.0, ""})
  test(calc.calculate("-72 - 7 - (1 - 2) * 3").value, {-76.0, ""})
  test(calc.calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").value, {-8.889, ""})
  test(calc.calculate("3^(1+1)^3"):map(function (p) return {math.floor(p[1] + 0.5), p[2]} end).value, {6561.0, ""})
  test(calc.calculate("sin(1+1)").value, {math.sin(2.0), ""})
  test(calc.calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").value, {-0.3634085731426532, ""})
  test(calc.calculate("sqr(2 + 3)").value, {25.0, ""})
  test(calc.calculate("sin(-PI/4)").value, {math.sin(-math.pi/4), ""}) -- -0.70710678118655
  test(calc.calculate(" E ^ PI").value, {23.140692632779267, ""})
  test(calc.calculate(" PI ^ E").value, {22.45915771836104, ""})
end

test_funcs()
test_consts()
testCalculator()
