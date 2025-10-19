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
    SomeParsers.token(parsers.reduce(Parser(A).empty){ |p, q| p.or_else{ q } })
  end

  def self.defObject(n : String, value : A) : Parser(A) forall A
    SomeParsers.name(n).skip{ Parser.pure(value) }
  end

  def self.funcs : Parser(Proc(Float64, Float64))
    fold([
      defObject("sin",   ->(x: Float64){ Math.sin(x) }),
      defObject("cos",   ->(x: Float64){ Math.cos(x) }),
      defObject("asin",  ->(x: Float64){ Math.asin(x) }),
      defObject("acos",  ->(x: Float64){ Math.acos(x) }),
      defObject("sinh",  ->(x: Float64){ Math.sinh(x) }),
      defObject("cosh",  ->(x: Float64){ Math.cosh(x) }),
      defObject("asinh", ->(x: Float64){ Math.asinh(x) }),
      defObject("acosh", ->(x: Float64){ Math.acosh(x) }),
      defObject("tan",   ->(x: Float64){ Math.tan(x) }),
      defObject("log",   ->(x: Float64){ Math.log(x) }),
      defObject("log10", ->(x: Float64){ Math.log10(x) }),
      defObject("exp",   ->(x: Float64){ Math.exp(x) }),
      defObject("sqrt",  ->(x: Float64){ Math.sqrt(x) }),
      defObject("sqr",   ->(x: Float64){ x * x })
    ])
  end

  def self.consts : Parser(Float64)
    fold([
      defObject("E",        Math::E),
      defObject("PI", 	    Math::PI),
      defObject("LOG2E",    1.44269504088896340736),  # log2(e)
      defObject("LOG10E",   0.434294481903251827651), # log10(e)
      defObject("LN2", 	    0.693147180559945309417), # ln(2)
      defObject("LN10",     2.30258509299404568402),  # ln(10)
      defObject("PI_2",     1.57079632679489661923),  # pi/2
      defObject("PI_4",     0.785398163397448309616), # pi/4
      defObject("1_PI",     0.318309886183790671538), # 1/pi
      defObject("2_PI",     0.636619772367581343076), # 2/pi
      defObject("2_SQRTPI", 1.12837916709551257390),  # 2/sqrt(pi)
      defObject("SQRT2",    1.41421356237309504880),  # sqrt(2)
      defObject("SQRT1_2",  0.707106781186547524401)  # 1/sqrt(2)
    ])
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
