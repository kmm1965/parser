let test_funcs = function(){
  let c = new Calculator();

  verify("calc(\"sin(2.0)\")", c.calc("sin(2.0)"), Maybe.Just([Math.sin(2.0), ""]));
  verify("calc(\"cos(2.0)\")", c.calc("cos(2.0)"), Maybe.Just([Math.cos(2.0), ""]));
  verify("calc(\"asin(0.5)\")", c.calc("asin(0.5)"), Maybe.Just([Math.asin(0.5), ""]));
  verify("calc(\"acos(0.5)\")", c.calc("acos(0.5)"), Maybe.Just([Math.acos(0.5), ""]));
  verify("calc(\"sinh(2.0)\")", c.calc("sinh(2.0)"), Maybe.Just([Math.sinh(2.0), ""]));
  verify("calc(\"cosh(2.0)\")", c.calc("cosh(2.0)"), Maybe.Just([Math.cosh(2.0), ""]));
  verify("calc(\"asinh(2.0)\")", c.calc("asinh(2.0)"), Maybe.Just([Math.asinh(2.0), ""]));
  verify("calc(\"acosh(2.0)\")", c.calc("acosh(2.0)"), Maybe.Just([Math.acosh(2.0), ""]));
  verify("calc(\"tan(2.0)\")", c.calc("tan(2.0)"), Maybe.Just([Math.tan(2.0), ""]));
  verify("calc(\"log(2.0)\")", c.calc("log(2.0)"), Maybe.Just([Math.log(2.0), ""]));
  verify("calc(\"log10(2.0)\")", c.calc("log10(2.0)"), Maybe.Just([Math.log10(2.0), ""]));
  verify("calc(\"exp(2.0)\")", c.calc("exp(2.0)"), Maybe.Just([Math.exp(2.0), ""]));
  verify("calc(\"sqrt(2.0)\")", c.calc("sqrt(2.0)"), Maybe.Just([Math.sqrt(2.0), ""]));
  verify("calc(\"sqr(2.0)\")", c.calc("sqr(2.0)"), Maybe.Just([4.0, ""]));
}

let test_consts = function(){
  let c = new Calculator();

  verify("calc(\"E\")", c.calc("E"), Maybe.Just([Math.E, ""]));
  verify("calc(\"LOG2E\")", c.calc("LOG2E"), Maybe.Just([1 / Math.log(2.0), ""]));
  verify("calc(\"LOG10E\")", c.calc("LOG10E"), Maybe.Just([0.4342944819032518, ""]));
  //verify("calc(\"LOG10E\")", c.calc("LOG10E"), Maybe.Just([1 / Math.log(10), ""]));
  verify("calc(\"LN2\")", c.calc("LN2"), Maybe.Just([Math.log(2.0), ""]));
  verify("calc(\"LN10\")", c.calc("LN10"), Maybe.Just([Math.log(10.0), ""]));
  verify("calc(\"PI\")", c.calc("PI"), Maybe.Just([Math.PI, ""]));
  verify("calc(\"PI_2\")", c.calc("PI_2"), Maybe.Just([Math.PI / 2, ""]));
  verify("calc(\"PI_4\")", c.calc("PI_4"), Maybe.Just([Math.PI / 4, ""]));
  verify("calc(\"1_PI\")", c.calc("1_PI"), Maybe.Just([1 / Math.PI, ""]));
  verify("calc(\"2_PI\")", c.calc("2_PI"), Maybe.Just([2 / Math.PI, ""]));
  verify("calc(\"2_SQRTPI\")", c.calc("2_SQRTPI"), Maybe.Just([2 / Math.sqrt(Math.PI), ""]));
  verify("calc(\"SQRT2\")", c.calc("SQRT2"), Maybe.Just([Math.sqrt(2), ""]));
  verify("calc(\"SQRT1_2\")", c.calc("SQRT1_2"), Maybe.Just([Math.sqrt(0.5), ""]));
}

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
