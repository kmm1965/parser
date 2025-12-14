require_relative 'SomeParsers.rb'

def op2(c, &f)
    symbol(c).skip { Parser.pure(f.call) }
end

def add = op2('+') { lambda { |x, y| x + y } }
def sub = op2('-') { lambda { |x, y| x - y } }
def mul = op2('*') { lambda { |x, y| x * y } }
def div = op2('/') { lambda { |x, y| x / y } }
def pow = op2('^') { lambda { |x, y| Math.exp(y * Math.log(x)) } }

def guard(b, &x)
    b ? Parser.pure(x.call) : Parser.empty
end

def funcs = identifier.and_then { |n|
    guard(n == "sin")   { lambda { |x| Math.sin(x) } }   |
    guard(n == "cos")   { lambda { |x| Math.cos(x) } }   |
    guard(n == "asin")  { lambda { |x| Math.asin(x) } }  |
    guard(n == "acos")  { lambda { |x| Math.acos(x) } }  |
    guard(n == "sinh")  { lambda { |x| Math.sinh(x) } }  |
    guard(n == "cosh")  { lambda { |x| Math.cosh(x) } }  |
    guard(n == "asinh") { lambda { |x| Math.asinh(x) } } |
    guard(n == "acosh") { lambda { |x| Math.acosh(x) } } |
    guard(n == "tan")   { lambda { |x| Math.tan(x) } }   |
    guard(n == "atan")  { lambda { |x| Math.atan(x) } }  |
    guard(n == "log")   { lambda { |x| Math.log(x) } }   |
    guard(n == "log10") { lambda { |x| Math.log10(x) } } |
    guard(n == "exp")   { lambda { |x| Math.exp(x) } }   |
    guard(n == "sqrt")  { lambda { |x| Math.sqrt(x) } }  |
    guard(n == "sqr")   { lambda { |x| x * x } }
}

def consts = identifier.and_then { |n|
    guard(n == "E")        { 2.71828182845904523536  } |
    guard(n == "PI")       { 3.14159265358979323846  } |
    guard(n == "LOG2E")    { 1.44269504088896340736  } | # log2(e)
    guard(n == "LOG10E")   { 0.434294481903251827651 } | # log10(e)
    guard(n == "LN2")      { 0.693147180559945309417 } | # ln(2)
    guard(n == "LN10")     { 2.30258509299404568402  } | # ln(10)
    guard(n == "PI_2")     { 1.57079632679489661923  } | # pi/2
    guard(n == "PI_4")     { 0.785398163397448309616 } | # pi/4
    guard(n == "1_PI")     { 0.318309886183790671538 } | # 1/pi
    guard(n == "2_PI")     { 0.636619772367581343076 } | # 2/pi
    guard(n == "2_SQRTPI") { 1.12837916709551257390  } | # 2/sqrt(pi)
    guard(n == "SQRT2")    { 1.41421356237309504880  } | # sqrt(2)
    guard(n == "SQRT1_2")  { 0.707106781186547524401 }   # 1/sqrt(2)
}

def expr = usign.and_then { |sgn| chainl1(term, add | sub, sgn == "-") }
def term = chainl1(factor, mul | div, false)
def factor = factor0.chainr1(pow)

def factor0 = expr_in_brackets |
    funcs.apply { expr_in_brackets } |
    consts |
    double

def expr_in_brackets = between(symbol('('), symbol(')')) { expr }

def calculate(inp)
    expr.parse(inp)
end
