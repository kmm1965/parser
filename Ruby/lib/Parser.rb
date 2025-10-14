require_relative 'Maybe'

class Parser
    def initialize(&unp)
        @unp = unp
    end
    
    def parse(inp)
        @unp.call(inp)
    end

    # Functor
    def transform(&f)
        self.and_then { |x| Parser.pure(f.call(x)) }
    end

    def transform_(f)
        self.transform { |x| f.call(x) }
    end

    # Applicative
    def self.pure(x)
        Parser.new { |inp| Just([x, inp]) }
    end

    def apply(&p)
        self.and_then { |f| p.call.transform { |x| f.call(x) } }
    end
    
    def *(p)
        self.apply { p }
    end
    
    # Monad
    def and_then(&f)
        Parser.new { |inp| self.parse(inp).and_then { |pair| f.call(pair[0]).parse(pair[1]) } }
    end
    
    def skip(&p)
        self.and_then { |_| p.call }
    end
    
    def >>(p)
        self.skip { p }
    end

    # Alternative
    def self.empty
        Parser.new { |_| Nothing }
    end
    
    def or_else(&f)
        Parser.new { |inp| self.parse(inp).or_else { f.call.parse(inp) } }
    end

    def |(p)
        self.or_else { p }
    end

    def some
        self.transform { |c| lambda { |s| c + s } }.apply { self.many }
    end
    
    def many
        self.some | empty_string
    end

    def token
        between(spaces, spaces){ self }
    end
    
    def rest_l(op, x)
        rest(lambda { |y| self.rest_l(op, y) }, op, x){ self }
    end

    def rest_r(op, x)
        rest(lambda { |y| Parser.pure(y) }, op, x){ self.chainr1(op) }
    end
    
    def chainr1(op)
        self.and_then { |x| self.rest_r(op, x) }
    end
end

def Parser_liftA2(f)
    lambda { |p1, p2| p1.transform { |x| f.call(x) } * p2.call }
end

def chainl1(p, op, negate_first)
    p.and_then { |x| p.rest_l(op, negate_first ? -x : x) }
end

def empty_string = Parser.pure("")

def anyChar = Parser.new { |inp| !inp.empty? ? Just([inp[0], inp[1..]]) : Nothing }

def satisfy(&pred)
    anyChar.and_then { |c| pred.call(c) ? Parser.pure(c) : Parser.empty }
end

def rest(ff, op, x, &p)
    op.and_then { |f| p.call.and_then { |y| ff.call(f.call(x, y)) } }.or_else { Parser.pure(x) }
end

def spaces = satisfy { |c| c.match(/\s/) != nil }.many

def between(open, close, &p)
    open.skip { p.call }.and_then { |x| close.skip { Parser.pure(x) } }
end
