require "../src/some_parsers"

class Calculator

  def self.op2(c : Char, f : Proc(Float64, Float64, Float64)) : Parser(Proc(Float64, Float64, Float64))
    SomeParsers.symbol(c).skip{ Parser.pure(f) }
  end

  def self.add
    op2('+', ->(x: Float64, y: Float64){ x + y })
  end

  def self.sub
    op2('-', ->(x: Float64, y: Float64){ x - y })
  end

  def self.mul
    op2('*', ->(x: Float64, y: Float64){ x * y })
  end

  def self.div
    op2('/', ->(x: Float64, y: Float64){ x / y })
  end

  def self.pow
    op2('^', ->(x: Float64, y: Float64){ Math.exp(y * Math.log(x)) })
  end

  def self.fold(parsers : Array(Parser(A))) : Parser(A) forall A
    parsers.reduce(Parser(A).empty){ |p, q| p.or_else{ q } }
  end

  def self.guard(b : Bool, value : A) : Parser(A) forall A
    b ? Parser.pure(value) : Parser(A).empty
  end

  def self.funcs : Parser(Proc(Float64, Float64))
    SomeParsers.identifier.flat_map{ |n| fold([
      guard(n == "sin",   ->(x: Float64){ Math.sin(x) }),
      guard(n == "cos",   ->(x: Float64){ Math.cos(x) }),
      guard(n == "asin",  ->(x: Float64){ Math.asin(x) }),
      guard(n == "acos",  ->(x: Float64){ Math.acos(x) }),
      guard(n == "sinh",  ->(x: Float64){ Math.sinh(x) }),
      guard(n == "cosh",  ->(x: Float64){ Math.cosh(x) }),
      guard(n == "asinh", ->(x: Float64){ Math.asinh(x) }),
      guard(n == "acosh", ->(x: Float64){ Math.acosh(x) }),
      guard(n == "tan",   ->(x: Float64){ Math.tan(x) }),
      guard(n == "log",   ->(x: Float64){ Math.log(x) }),
      guard(n == "log10", ->(x: Float64){ Math.log10(x) }),
      guard(n == "exp",   ->(x: Float64){ Math.exp(x) }),
      guard(n == "sqrt",  ->(x: Float64){ Math.sqrt(x) }),
      guard(n == "sqr",   ->(x: Float64){ x * x })
    ]) }
  end

  def self.consts : Parser(Float64)
    SomeParsers.identifier.flat_map{ |n| fold([
      guard(n == "E",        Math::E),
      guard(n == "PI", 	    Math::PI),
      guard(n == "LOG2E",    1.44269504088896340736),  # log2(e)
      guard(n == "LOG10E",   0.434294481903251827651), # log10(e)
      guard(n == "LN2", 	    0.693147180559945309417), # ln(2)
      guard(n == "LN10",     2.30258509299404568402),  # ln(10)
      guard(n == "PI_2",     1.57079632679489661923),  # pi/2
      guard(n == "PI_4",     0.785398163397448309616), # pi/4
      guard(n == "1_PI",     0.318309886183790671538), # 1/pi
      guard(n == "2_PI",     0.636619772367581343076), # 2/pi
      guard(n == "2_SQRTPI", 1.12837916709551257390),  # 2/sqrt(pi)
      guard(n == "SQRT2",    1.41421356237309504880),  # sqrt(2)
      guard(n == "SQRT1_2",  0.707106781186547524401)  # 1/sqrt(2)
    ]) }
  end

  def self.expr() : Parser(Float64)
    SomeParsers.usign.flat_map{ |sgn| SomeParsers.chainl1(term, add.or_else{ sub }, sgn == "-") }
  end

  def self.term() : Parser(Float64)
    SomeParsers.chainl1(factor, mul.or_else{ div }, false)
  end

  def self.factor() : Parser(Float64)
    SomeParsers.chainr1(factor0, pow)
  end

  def self.factor0() : Parser(Float64)
    expr_in_brackets
      .or_else{ Parser.apply(funcs){ expr_in_brackets } }
      .or_else{ consts }
      .or_else{ SomeParsers.double }
  end

  def self.expr_in_brackets() : Parser(Float64)
    SomeParsers.between(SomeParsers.symbol('('), SomeParsers.symbol(')')){ expr }
  end

  def self.calculate(s : String) : Maybe({Float64, String})
    expr.parse(s)
  end

end
