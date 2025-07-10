let test_calculator = function(){
  let c = new Calculator();

  verify("calc(\"72 - 7 - (1 - 2) * 3\")", c.calc("72 - 7 - (1 - 2) * 3"), Maybe.Just([68, ""]));
  verify("calc(\" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)\")", c.calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"), Maybe.Just([-8.889, ""]));
  verify("calc(\"3^(1+1)^3\")", c.calc("3^(1+1)^3").map(([x, s]) => [Math.round(x), s]), Maybe.Just([6561, ""]));
  verify("calc(\"sin(1+1)\")", c.calc("sin(1+1)"), Maybe.Just([Math.sin(2), ""]));
  verify("calc(\"sqr(sin(2)) + sqr(cos(2))\")", c.calc("sqr(sin(2)) + sqr(cos(2))"), Maybe.Just([1, ""]));
  verify("calc(\"sin ( 2_SQRTPI * sqr ( 2 ) - 1 )", c.calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"), Maybe.Just([-0.3634085731426532, ""]));
  verify("calc(\"sqr(2 + 3)", c.calc("sqr(2 + 3)"), Maybe.Just([25, ""]));
  verify("calc(\"sin(-PI/4)", c.calc("sin(-PI/4)"), Maybe.Just([-0.7071067811865475, ""]));
  verify("calc(\" E ^ PI", c.calc(" E ^ PI"), Maybe.Just([23.140692632779267, ""]));
  verify("calc(\" PI ^ E", c.calc(" PI ^ E"), Maybe.Just([22.45915771836104, ""]));
}
