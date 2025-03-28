class Maybe
    def initialize(value)
        @value = value
    end
    
    def value
        @value
    end
    
    # Functor
    def transform(&f)
        Maybe.new(@value.map { |x| f.call(x) })
    end

    def transform_(f)
        self.transform { |x| f.call(x) }
    end

    # Applicative
    def self.pure(x)
        Just(x)
    end

    def apply(&m)
        self.and_then { |f| m.call.transform { |x| f.call(x) } }
    end
    
    def *(m)
        self.apply { m }
    end
    
    # Monad
    def and_then(&f)
        @value.empty? ? Nothing : f.call(@value[0])
    end
    
    def skip(&m)
        self.and_then { |_| m.call }
    end
    
    def >>(m)
        self.skip { m }
    end

    # Alternative
    def self.empty
        Nothing
    end
    
    def or_else(&f)
        !@value.empty? ? self : f.call
    end

    def |(m)
        self.or_else { m }
    end
end

def Just(x)
    Maybe.new([x])
end

Nothing = Maybe.new([])

def Maybe_liftA2(f)
    lambda { |m1, m2| m1.transform { |x| f.call(x) } * m2 }
end
