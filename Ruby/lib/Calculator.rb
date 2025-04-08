require_relative 'SomeParsers.rb'

def sqr(x)
    x * x
end

def def_object(n, &x)
    name_(n).skip { Parser.pure(x.call) }
end

def func =
    def_object("sin")   { lambda { |x| Math.sin(x) } }   |
    def_object("cos")   { lambda { |x| Math.cos(x) } }   |
    def_object("asin")  { lambda { |x| Math.asin(x) } }  |
    def_object("acos")  { lambda { |x| Math.acos(x) } }  |
    def_object("sinh")  { lambda { |x| Math.sinh(x) } }  |
    def_object("cosh")  { lambda { |x| Math.cosh(x) } }  |
    def_object("asinh") { lambda { |x| Math.asinh(x) } } |
    def_object("acosh") { lambda { |x| Math.acosh(x) } } |
    def_object("tan")   { lambda { |x| Math.tan(x) } }   |
    def_object("atan")  { lambda { |x| Math.atan(x) } }  |
    def_object("log")   { lambda { |x| Math.log(x) } }   |
    def_object("exp")   { lambda { |x| Math.exp(x) } }   |
    def_object("sqrt")  { lambda { |x| Math.sqrt(x) } }  |
    def_object("sqr")   { lambda { |x| sqr(x) } }

def m_E        = 2.71828182845904523536   # e
def m_LOG2E    = 1.44269504088896340736   # log2(e)
def m_LOG10E   = 0.434294481903251827651  # log10(e)
def m_LN2      = 0.693147180559945309417  # ln(2)
def m_LN10     = 2.30258509299404568402   # ln(10)
def m_PI       = 3.14159265358979323846   # pi
def m_PI_2     = 1.57079632679489661923   # pi/2
def m_PI_4     = 0.785398163397448309616  # pi/4
def m_1_PI     = 0.318309886183790671538  # 1/pi
def m_2_PI     = 0.636619772367581343076  # 2/pi
def m_2_SQRTPI = 1.12837916709551257390   # 2/sqrt(pi)
def m_SQRT2    = 1.41421356237309504880   # sqrt(2)
def m_SQRT1_2  = 0.707106781186547524401  # 1/sqrt(2)

def const =
    def_object("E")        { m_E }        |
    def_object("LOG2E")    { m_LOG2E }    |
    def_object("LOG10E")   { m_LOG10E }   |
    def_object("LN2")      { m_LN2 }      |
    def_object("LN10")     { m_LN10 }     |
    def_object("PI")       { m_PI }       |
    def_object("PI_2")     { m_PI_2 }     |
    def_object("PI_4")     { m_PI_4 }     |
    def_object("1_PI")     { m_1_PI }     |
    def_object("2_PI")     { m_2_PI }     |
    def_object("2_SQRTPI") { m_2_SQRTPI } |
    def_object("SQRT2")    { m_SQRT2 }    |
    def_object("SQRT1_2")  { m_SQRT1_2 }

def op2(c, &f)
    symbol(c).skip { Parser.pure(f.call) }
end

def add = op2('+') { lambda { |x, y| x + y } }
def sub = op2('-') { lambda { |x, y| x - y } }
def mul = op2('*') { lambda { |x, y| x * y } }
def div = op2('/') { lambda { |x, y| x / y } }
def pow = op2('^') { lambda { |x, y| Math.exp(y * Math.log(x)) } }

def expr = usign.and_then { |sgn| chainl1(term, add | sub, sgn == "-") }
def term = chainl1(factor, mul | div, false)
def factor = factor0.chainr1(pow)

def factor0 = expr_in_brackets |
    func.apply { expr_in_brackets } |
    const |
    double

def expr_in_brackets = between(symbol('('), symbol(')')) { expr }

def calculate(inp)
    expr.parse(inp)
end
