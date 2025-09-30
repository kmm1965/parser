require "../src/maybe"

class Parser(T)
  getter &unp : String -> Maybe({T, String})

  def initialize(&unp : String -> Maybe({T, String}))
    @unp = unp
  end

  def parse(inp : String) : Maybe({T, String})
    @unp.call(inp)
  end

  def self.pure(x : T) : Parser(T)
    Parser(T).new{ |inp| Just.new({x, inp}) }
  end

  def self.empty() : Parser(T)
    Parser(T).new{ |inp| Nothing({T, String}).new }
  end

  def map(&f : T -> U) : Parser(U) forall U
    Parser(U).new{ |inp| self.parse(inp).map{ |p| {f.call(p[0]), p[1]} } }
  end

  def flat_map(&f : T -> Parser(U)) : Parser(U) forall U
    Parser(U).new{ |inp| self.parse(inp).flat_map{ |p| f.call(p[0]).parse(p[1]) } }
  end

  def skip(&fp : -> Parser(U)) : Parser(U) forall U
    self.flat_map{ |_| fp.call }
  end

  def self.apply(pf : Parser(Proc(T, U)), &fp : -> Parser(T)) : Parser(U) forall U
    pf.flat_map{ |f| fp.call.map{ |x| f.call(x) } }
  end

  def or_else(&fp : -> Parser(T)) : Parser(T)
    Parser(T).new{ |inp| self.parse(inp).or_else{ fp.call.parse(inp) } }
  end

end
