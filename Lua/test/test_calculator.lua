local calc = require("../src/calculator")

function test(value, expected)
  print(expected == nil and value == expected or value ~= nil and value[1] == expected[1] and value[2] == expected[2])
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

testCalculator()
