require "../src/parser"

class SomeParsers
  def self.empty_string
    Parser.pure("")
  end

  def self.some(p : Parser(Char)) : Parser(String)
    Parser.apply(p.map{ |c| ->(s : String){ c.to_s + s } }){ many(p) }
  end

  def self.many(p : Parser(Char)) : Parser(String)
    some(p).or_else{empty_string}
  end

  def self.anyChar : Parser(Char)
    Parser(Char).new{|inp| inp.size > 0 ? Just.new({inp[0], inp[1..-1]}) : Nothing({Char, String}).new}
  end

  def self.satisfy(&pred : Char -> Bool) : Parser(Char)
    anyChar().flat_map{ |c| pred.call(c) ? Parser.pure(c) : Parser(Char).empty }
  end

  def self.char(c : Char) : Parser(Char)
    satisfy{ |x| x == c }
  end

  def self.spaces : Parser(String)
    many(satisfy( &.whitespace? ))
  end

  def self.between(open : Parser(O), close : Parser(C), &fp : -> Parser(A)) : Parser(A) forall O, C, A
    open.skip{ fp.call.flat_map{ |x| close.skip{ Parser.pure(x) } } }
  end

  def self.token(p : Parser(A)) : Parser(A) forall A
    between(spaces, spaces){ p }
  end

  def self.symbol(c : Char) : Parser(Char)
    token(char(c))
  end

  def self.alnum : Parser(Char)
    satisfy{ |c| c.alphanumeric? || c == '_' }
  end

  def self.name(n : String) : Parser(String)
    token(some(alnum).flat_map{ |n2| n2 == n ? Parser.pure(n) : Parser(String).empty })
  end

  def self.optional_s(p : Parser(String)) : Parser(String)
    p.or_else{ empty_string }
  end

  def self.optional_c(p : Parser(Char)) : Parser(String)
    optional_s(p.map(&.to_s))
  end

  def self.digit : Parser(Char)
    satisfy(&.number?)
  end

  def self.digits : Parser(String)
    many(digit)
  end

  def self.sign : Parser(String)
    optional_c(char('+').or_else{ char('-') })
  end

  def self.usign : Parser(String)
    optional_c(symbol('+').or_else{ symbol('-') })
  end

  def self.double : Parser(Float64)
    token(digits
      .flat_map{ |int_part| optional_s(char('.').skip{ digits })
      .flat_map{ |frac_part| optional_s(char('e').or_else{ char('E') }.skip{ sign }
        .flat_map{ |exp_sign| some(digit)
        .flat_map{ |exp_digits| Parser.pure(exp_sign + exp_digits) } })
      .flat_map{ |exp_part| int_part.size > 0 || frac_part.size > 0 ?
        Parser.pure((int_part +
          (frac_part.size > 0 ? "." + frac_part : "") +
          (exp_part.size > 0 ? "e" + exp_part : "")).to_f)
        : Parser(Float64).empty } } })
  end

  def self.rest(op : Parser(Proc(A, A, A)), x : A, ff : Proc(A, Parser(A)), &fp : -> Parser(A)) : Parser(A) forall A 
    op.flat_map{ |f| fp.call.flat_map{ |y| ff.call(f.call(x, y)) } }.or_else{ Parser.pure(x) }
  end

  def self.rest_l(p : Parser(A), op : Parser(Proc(A, A, A)), x : A) : Parser(A) forall A
    rest(op, x, ->(y: A){ rest_l(p, op, y) }){ p }
  end

  def self.chainl1(p : Parser(Float64), op : Parser(Proc(Float64, Float64, Float64)), negate_first : Bool) : Parser(Float64)
    p.flat_map{ |x| rest_l(p, op, negate_first ? -x : x) }
  end

  def self.rest_r(p : Parser(A), op : Parser(Proc(A, A, A)), x : A) : Parser(A) forall A
    rest(op, x, ->(y: A){ Parser.pure(y) }){ chainr1(p, op) }
  end

  def self.chainr1(p : Parser(A), op : Parser(Proc(A, A, A))) : Parser(A) forall A
    p.flat_map{ |x| rest_r(p, op, x) }
  end

end
