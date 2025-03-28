require 'Parser'

class TestParser < Test::Unit::TestCase

    # Functor
    def test_transform
        fi = lambda { |x| x.to_s }
        fs = lambda { |s| s.to_i }

        assert_equal(Parser.pure(1).transform { |x| x.to_s }.parse("abc").value, [["1", "abc"]])
        assert_equal(Parser.pure(1.0).transform_(fi).parse("abc").value, [["1.0", "abc"]])
        assert_equal(Parser.pure("1").transform_(fs).parse("abc").value, [[1, "abc"]])

        assert_equal(Parser.empty.transform { |x| x.to_s }.parse("abc").value, [])
        assert_equal(Parser.empty.transform_(fi).parse("abc").value, [])
        assert_equal(Parser.empty.transform_(fs).parse("abc").value, [])
    end

    # Applicative
    def test_pure
        assert_equal(Parser.pure(1).parse("abc").value, [[1, "abc"]])
        assert_equal(Parser.pure(1.0).parse("abc").value, [[1.0, "abc"]])
        assert_equal(Parser.pure("1").parse("abc").value, [["1", "abc"]])
    end
    
    def test_apply
        psin = Parser.pure(lambda { |x| Math.sin(x) })
        p1 = Parser.pure(1)
        pempty = Parser.empty
        
        assert_equal((psin * p1).parse("abc").value, [[Math.sin(1), "abc"]])
        assert_equal((psin * pempty).parse("abc").value, [])
        assert_equal((pempty * p1).parse("abc").value, [])
        assert_equal((pempty * pempty).parse("abc").value, [])

        assert_equal(psin.apply { p1 }.parse("abc").value, [[Math.sin(1), "abc"]])
        assert_equal(psin.apply { pempty }.parse("abc").value, [])
        assert_equal(pempty.apply { p1 }.parse("abc").value, [])
        assert_equal(pempty.apply { pempty }.parse("abc").value, [])
    end
    
    # Monad
    def test_and_then
        p1 = Parser.pure(1)
        pempty = Parser.empty

        eat    = lambda { |x| Parser.new { |inp| Just([x.to_s + inp, ""]) } }
        cancel = lambda { |_| Parser.new { |_| Nothing } }

        assert_equal(p1.and_then { |x| eat.call(x) }.parse("abc").value, [["1abc", ""]])
        assert_equal(p1.and_then { |x| cancel.call(x) }.parse("abc").value, [])

        assert_equal(pempty.and_then { |x| eat.call(x) }.parse("abc").value, [])
        assert_equal(pempty.and_then { |x| cancel.call(x) }.parse("abc").value, [])
    end

    def test_anyChar
        assert_equal(anyChar.parse("abc").value, [['a', "bc"]])
        assert_equal(anyChar.parse("").value, [])
    end

    def test_empty_string
        assert_equal(empty_string.parse("abc").value, [["", "abc"]])
    end
    
    def test_satisfy
        assert_equal(satisfy { |c| c == 'a' }.parse("abc").value, [['a', "bc"]])
        assert_equal(satisfy { |c| c == 'z' }.parse("abc").value, [])
    end
    
    def test_spaces
        assert_equal(spaces.parse("abc").value, [["", "abc"]])
        assert_equal(spaces.parse("  abc").value, [["  ", "abc"]])
    end
    
end
