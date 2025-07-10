require 'SomeParsers'

class TestSomeParsers < Test::Unit::TestCase

    def test_alnum
        assert_equal(alnum.parse("123abc  ").value, [['1', "23abc  "]])
        assert_equal(alnum.parse("_123  abc").value, [['_', "123  abc"]])
        assert_equal(alnum.parse("!@%$").value, [])
    end
    
    def test_char
        assert_equal(char('a').parse("abc").value, [['a', "bc"]])
        assert_equal(char('z').parse("abc").value, [])
    end

    def test_symbol
        assert_equal(symbol('+').parse(" + abc").value, [['+', "abc"]])
        assert_equal(symbol('+').parse("abc").value, [])
    end

    def test_name
        nsin = name_("sin")
        
        assert_equal(nsin.parse(" sin ").value, [["sin", ""]])
        assert_equal(nsin.parse("  sin  (1.)").value, [["sin", "(1.)"]])
        assert_equal(nsin.parse("sinabc").value, [])
    end

    def test_optional_s
        assert_equal(optional_s(name_("sin")).parse(" sin abc").value, [["sin", "abc"]])
        assert_equal(optional_s(name_("sin")).parse("abc").value, [["", "abc"]])
        assert_equal(optional_s(char('1')).parse("1234").value, [["1", "234"]])
        assert_equal(optional_s(char('1')).parse("abc").value, [["", "abc"]])
    end
    
    def test_sign
        assert_equal(sign.parse("abc").value, [["", "abc"]])
        assert_equal(sign.parse("+abc").value, [['+', "abc"]])
        assert_equal(sign.parse("-abc").value, [['-', "abc"]])
    end

    def test_digits
        assert_equal(digits.parse("123abc").value, [["123", "abc"]])
        assert_equal(digits.parse("123  abc").value, [["123", "  abc"]])
        assert_equal(digits.parse("abc").value, [["", "abc"]])
    end

    def test_double
        assert_equal(double.parse(" 1 abc").value, [[1, "abc"]])
        assert_equal(double.parse(" 1. abc").value, [[1, "abc"]])
        assert_equal(double.parse(" 1.23 abc").value, [[1.23, "abc"]])
        assert_equal(double.parse(" .23 abc").value, [[0.23, "abc"]])
        assert_equal(double.parse("1.23e10abc").value, [[1.23e10, "abc"]])
        assert_equal(double.parse("1.23e-10abc").value, [[1.23e-10, "abc"]])
        assert_equal(double.parse("abc").value, [])
    end

    def test_between
        expr = between(symbol('('), symbol(')')) { double }
        
        assert_equal(expr.parse(" ( 123 ) abc").value, [[123, "abc"]])
        assert_equal(expr.parse(" ( 123 abc").value, [])
        assert_equal(expr.parse(" 123 ) abc").value, [])
    end

    def test_chainlr1
        add = symbol('+').skip { Parser.pure(lambda { |x, y| x + y }) }
        sub = symbol('-').skip { Parser.pure(lambda { |x, y| x - y }) }
        pow = symbol('^').skip { Parser.pure(lambda { |x, y| Math.exp(y * Math.log(x)) }) }

        pexpr = chainl1(double, add | sub, false)

        assert_equal(pexpr.parse("7").value, [[7, ""]])
        assert_equal(pexpr.parse("7abc").value, [[7, "abc"]])
        assert_equal(pexpr.parse("7-1").value, [[6, ""]])
        assert_equal(pexpr.parse(" 7 - 1 - 2 abc").value, [[4, "abc"]])
        assert_equal(pexpr.parse(" 7 - 1 + 2 - 3 abc").value, [[5, "abc"]])
        assert_equal(pexpr.parse("abc").value, [])

        assert_equal(double.chainr1(pow).parse("3 ^ 2 ^ 3 abc")
            .transform { |pair| [pair[0].round, pair[1]] }.value, [[6561, "abc"]])
    end

end
