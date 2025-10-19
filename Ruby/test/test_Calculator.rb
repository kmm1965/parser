require 'Calculator'

class TestCalculator < Test::Unit::TestCase

    def test_Funcs
        assert_equal(calculate("sin(2.0)").value, [[Math.sin(2.0), ""]])
        assert_equal(calculate("cos(2.0)").value, [[Math.cos(2.0), ""]])
        assert_equal(calculate("asin(0.5)").value, [[Math.asin(0.5), ""]])
        assert_equal(calculate("acos(0.5)").value, [[Math.acos(0.5), ""]])
        assert_equal(calculate("sinh(2.0)").value, [[Math.sinh(2.0), ""]])
        assert_equal(calculate("cosh(2.0)").value, [[Math.cosh(2.0), ""]])
        assert_equal(calculate("asinh(2.0)").value, [[Math.asinh(2.0), ""]])
        assert_equal(calculate("acosh(2.0)").value, [[Math.acosh(2.0), ""]])
        assert_equal(calculate("tan(2.0)").value, [[Math.tan(2.0), ""]])
        assert_equal(calculate("log(2.0)").value, [[Math.log(2.0), ""]])
        assert_equal(calculate("log10(2.0)").value, [[Math.log10(2.0), ""]])
        assert_equal(calculate("exp(2.0)").value, [[Math.exp(2.0), ""]])
        assert_equal(calculate("sqrt(2.0)").value, [[Math.sqrt(2.0), ""]])
        assert_equal(calculate("sqr(2.0)").value, [[4.0, ""]])
    end

    def test_Consts
      pi = 3.14159265358979323846
      assert_equal(calculate("E").value, [[2.7182818284590452, ""]])
      assert_equal(calculate("LOG2E").value, [[1 / Math.log(2.0), ""]])
      assert_equal(calculate("LOG10E").value, [[0.4342944819032518, ""]])
      #assert_equal(calculate("LOG10E").value, [[1 / Math.log(10.0), ""]])
      assert_equal(calculate("LN2").value, [[Math.log(2.0), ""]])
      assert_equal(calculate("LN10").value, [[Math.log(10.0), ""]])
      assert_equal(calculate("PI").value, [[pi, ""]])
      assert_equal(calculate("PI_2").value, [[pi / 2, ""]])
      assert_equal(calculate("PI_4").value, [[pi / 4, ""]])
      assert_equal(calculate("1_PI").value, [[1 / pi, ""]])
      assert_equal(calculate("2_PI").value, [[2 / pi, ""]])
      assert_equal(calculate("2_SQRTPI").value, [[2 / Math.sqrt(pi), ""]])
      assert_equal(calculate("SQRT2").value, [[Math.sqrt(2), ""]])
      assert_equal(calculate("SQRT1_2").value, [[Math.sqrt(0.5), ""]])
    end

    def test_Calculator
        assert_equal(calculate("72 - 7 - (1 - 2) * 3").value, [[68.0, ""]])
        assert_equal(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").value, [[-8.889, ""]])
        assert_equal(calculate("3^(1+1)^3").transform { |pair| [pair[0].round, pair[1]] }.value, [[6561.0, ""]])
        assert_equal(calculate("sin(1+1)").value, [[Math.sin(2), ""]])
        assert_equal(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").value, [[-0.3634085731426532, ""]])
        assert_equal(calculate("sqr(2 + 3)").value, [[25, ""]])
        assert_equal(calculate("E ^ PI").value, [[23.140692632779267, ""]])
        assert_equal(calculate("PI ^ E").value, [[22.45915771836104, ""]])
    end
end
