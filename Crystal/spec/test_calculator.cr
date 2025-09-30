require "spec"
require "../src/calculator"

describe Calculator do
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
