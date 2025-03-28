require 'Maybe'

class TestMaybe < Test::Unit::TestCase
    
    def test_Just_Nothing
        assert_equal(Just(3).value, [3])
        assert_equal(Nothing.value, [])
    end

    # Functor
    def test_transform
        fsin = lambda { |x| Math.sin(x) }
        
        assert_equal(Just(1).transform { |x| Math.sin(x) }.value, [Math.sin(1)])
        assert_equal(Nothing.transform { |x| Math.sin(x) }.value, [])

        assert_equal(Just(1).transform_(fsin).value, [Math.sin(1)])
        assert_equal(Nothing.transform_(fsin).value, [])

        assert_equal(Just(1).transform { |x| x.to_s }.value, ["1"])
        assert_equal(Nothing.transform { |x| x.to_s }.value, [])
    end

    # Applicative
    def test_pure
        assert_equal(Maybe.pure(3).value, [3])
    end

    def test_apply
        minus = lambda { |x| lambda { |y| x - y } }

        assert_equal(Maybe_liftA2(minus).call(Just(8), Just(3)).value, [5])
        assert_equal(Maybe_liftA2(minus).call(Nothing, Just(8)).value, [])
        assert_equal(Maybe_liftA2(minus).call(Just(8), Nothing).value, [])
        assert_equal(Maybe_liftA2(minus).call(Nothing, Nothing).value, [])

        assert_equal((Just(8).transform { |x| lambda { |y| x - y } } * Just(3)).value, [5])
        assert_equal((Just(8).transform { |x| lambda { |y| x - y } } * Nothing).value, [])
        assert_equal((Nothing.transform { |x| lambda { |y| x - y } } * Just(3)).value, [])
        assert_equal((Nothing.transform { |x| lambda { |y| x - y } } * Nothing).value, [])

        assert_equal(Just(8).transform { |x| lambda { |y| x - y } }.apply { Just(3) }.value, [5])
        assert_equal(Just(8).transform { |x| lambda { |y| x - y } }.apply { Nothing }.value, [])
        assert_equal(Nothing.transform { |x| lambda { |y| x - y } }.apply { Just(3) }.value, [])
        assert_equal(Nothing.transform { |x| lambda { |y| x - y } }.apply { Nothing }.value, [])

        assert_equal((Just(8).transform_(minus) * Just(3)).value, [5])
        assert_equal((Just(8).transform_(minus) * Nothing).value, [])
        assert_equal((Nothing.transform_(minus) * Just(3)).value, [])
        assert_equal((Nothing.transform_(minus) * Nothing).value, [])

        assert_equal(Just(8).transform_(minus).apply { Just(3) }.value, [5])
        assert_equal(Just(8).transform_(minus).apply { Nothing }.value, [])
        assert_equal(Nothing.transform_(minus).apply { Just(3) }.value, [])
        assert_equal(Nothing.transform_(minus).apply { Nothing }.value, [])
    end

    # Monad
    def test_and_then
        def safe_sqrt(x)
            x >= 0 ? Just(Math.sqrt(x)) : Nothing
        end
        
        def safe_log(x)
            x > 0 ? Just(Math.log(x)) : Nothing
        end
        
        assert_equal(safe_sqrt(2).value, [Math.sqrt(2)])
        assert_equal(safe_sqrt(2).and_then { |x| safe_log(x) }.value, [Math.log(Math.sqrt(2))])
        assert_equal(safe_sqrt(0).and_then { |x| safe_log(x) }.value, [])
        assert_equal(safe_sqrt(-2).and_then { |x| safe_log(x) }.value, [])

        assert_equal(Just(2).and_then { |x| safe_sqrt(x) }.value, [Math.sqrt(2)])
        assert_equal(Just(2).and_then { |x| safe_sqrt(x) }.and_then { |x| safe_log(x) }.value, [Math.log(Math.sqrt(2))])
        assert_equal(Just(0).and_then { |x| safe_log(x) }.value, [])
        assert_equal(Just(-2).and_then { |x| safe_sqrt(x) }.value, [])

        assert_equal(Nothing.and_then { |x| safe_sqrt(x) }.value, [])
        assert_equal(Nothing.and_then { |x| safe_sqrt(x) }.and_then { |x| safe_log(x) }.value, [])

        assert_equal(Just(2).and_then { |x| x % 2 == 0 ? Just(x.to_s) : Nothing }.value, ["2"])
        assert_equal(Just(1).and_then { |x| x % 2 == 0 ? Just(x.to_s) : Nothing }.value, [])
        assert_equal(Nothing.and_then { |x| x % 2 == 0 ? Just(x.to_s) : Nothing }.value, [])
    end

    # Alternative
    def test_or_else
        assert_equal(Just(2).or_else { Just(3) }.value, [2])
        assert_equal(Just(2).or_else { Nothing }.value, [2])

        assert_equal(Nothing.or_else { Just(3) }.value, [3])
        assert_equal(Nothing.or_else { Nothing }.value, [])

        assert_equal((Just(2) | Just(3)).value, [2])
        assert_equal((Just(2) | Nothing).value, [2])

        assert_equal((Nothing | Just(3)).value, [3])
        assert_equal((Nothing | Nothing).value, [])
    end
    
end
