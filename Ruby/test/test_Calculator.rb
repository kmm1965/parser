require 'Calculator'

class TestCalculator < Test::Unit::TestCase

    def test_Calculator
        assert_equal(calculate("72 - 7 - (1 - 2) * 3").value, [[68, ""]])
        assert_equal(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").value, [[-8.889, ""]])
        assert_equal(calculate("3^(1+1)^3").transform { |pair| [pair[0].round, pair[1]] }.value, [[6561, ""]])
        assert_equal(calculate("sin(1+1)").value, [[Math.sin(2), ""]])
        assert_equal(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").value, [[-0.3634085731426532, ""]])
        assert_equal(calculate("sqr(2 + 3)").value, [[25, ""]])
        assert_equal(calculate("E ^ PI").value, [[23.140692632779267, ""]])
        assert_equal(calculate("PI ^ E").value, [[22.45915771836104, ""]])
    end
    
end
