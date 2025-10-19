require "spec"
require "../src/calculator"

describe Calculator do
  describe "funcs" do
    it "test funcs" do
      Calculator.calculate("sin(2.0)").should be Just.new({Math.sin(2.0), ""})
      Calculator.calculate("cos(2.0)").should be Just.new({Math.cos(2.0), ""})
      Calculator.calculate("asin(0.5)").should be Just.new({Math.asin(0.5), ""})
      Calculator.calculate("acos(0.5)").should be Just.new({Math.acos(0.5), ""})
      Calculator.calculate("sinh(2.0)").should be Just.new({Math.sinh(2.0), ""})
      Calculator.calculate("cosh(2.0)").should be Just.new({Math.cosh(2.0), ""})
      Calculator.calculate("asinh(2.0)").should be Just.new({Math.asinh(2.0), ""})
      Calculator.calculate("acosh(2.0)").should be Just.new({Math.acosh(2.0), ""})
      Calculator.calculate("tan(2.0)").should be Just.new({Math.tan(2.0), ""})
      Calculator.calculate("log(2.0)").should be Just.new({Math.log(2.0), ""})
      Calculator.calculate("log10(2.0)").should be Just.new({Math.log10(2.0), ""})
      Calculator.calculate("exp(2.0)").should be Just.new({Math.exp(2.0), ""})
      Calculator.calculate("sqrt(2.0)").should be Just.new({Math.sqrt(2.0), ""})
      Calculator.calculate("sqr(2.0)").should be Just.new({4.0, ""})
    end
  end

  describe "consts" do
    it "test consts" do
      Calculator.calculate("E").should be Just.new({Math::E, ""})
      Calculator.calculate("LOG2E").should be Just.new({1 / Math.log(2.0), ""})
      Calculator.calculate("LOG10E").should be Just.new({0.4342944819032518, ""})
      #Calculator.calculate("LOG10E").should be Just.new({1 / Math.log(10.0), ""})
      Calculator.calculate("LN2").should be Just.new({Math.log(2.0), ""})
      Calculator.calculate("LN10").should be Just.new({Math.log(10.0), ""})
      Calculator.calculate("PI").should be Just.new({Math::PI, ""})
      Calculator.calculate("PI_2").should be Just.new({Math::PI / 2, ""})
      Calculator.calculate("PI_4").should be Just.new({Math::PI / 4, ""})
      Calculator.calculate("1_PI").should be Just.new({1 / Math::PI, ""})
      Calculator.calculate("2_PI").should be Just.new({2 / Math::PI, ""})
      Calculator.calculate("2_SQRTPI").should be Just.new({2 / Math.sqrt(Math::PI), ""})
      Calculator.calculate("SQRT2").should be Just.new({Math.sqrt(2), ""})
      Calculator.calculate("SQRT1_2").should be Just.new({Math.sqrt(0.5), ""})
    end
  end

  describe "calculate" do
    it "test calculate" do
      Calculator.calculate("72 - 7 - (1 - 2) * 3").should be Just.new({68.0, ""})
      Calculator.calculate("-72 - 7 - (1 - 2) * 3").should be Just.new({-76.0, ""})
      Calculator.calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").should be Just.new({-8.889, ""})
      Calculator.calculate("3^(1+1)^3").map{ |p| {(p[0] + 0.5).to_i.to_f, p[1]} }.should be Just.new({6561.0, ""})
      Calculator.calculate("sin(1+1)").should be Just.new({Math.sin(2.0), ""})
      Calculator.calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").should be Just.new({-0.3634085731426532, ""})
      Calculator.calculate("sqr(2 + 3)").should be Just.new({25.0, ""})
      Calculator.calculate("sin(-PI/4)").should be Just.new({Math.sin(-Math::PI/4.0), ""})
      Calculator.calculate(" E ^ PI").should be Just.new({23.140692632779267, ""})
      Calculator.calculate(" PI ^ E").should be Just.new({22.45915771836104, ""})
    end
  end
end
