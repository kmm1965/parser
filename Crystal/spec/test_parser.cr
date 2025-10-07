require "spec"
require "../src/parser"

def eat(i : Int32)
  Parser(String).new{|inp| Just.new({i.to_s + inp, ""})}
end

def cancel(i : Int32)
  Parser(String).empty
end

describe Parser do
  describe "pure" do
    it "test Parser.pure" do
      Parser.pure(1).parse("abc").should be Just.new({1, "abc"})
      Parser.pure(1.0).parse("abc").should be Just.new({1.0, "abc"})
      Parser.pure("1").parse("abc").should be Just.new({"1", "abc"})
    end
  end

  describe "map" do
    it "test Parser.map" do
      Parser.pure(1).map(&.to_s).parse("abc").should be Just.new({"1", "abc"})
      Parser.pure(1.0).map(&.to_s).parse("abc").should be Just.new({"1.0", "abc"})
      Parser.pure("1").map(&.to_i32).parse("abc").should be Just.new({1, "abc"})

      Parser(Int32).empty.map(&.to_s).parse("abc").should be Nothing({String, String}).new
      Parser(Float64).empty.map(&.to_s).parse("abc").should be Nothing({String, String}).new
      Parser(String).empty.map(&.to_i32).parse("abc").should be Nothing({Int32, String}).new
    end
  end

  describe "flat_map" do
    it "test Parser.flat_map" do
      i1 = Parser.pure(1)
      empty = Parser(Int32).empty

      i1.flat_map(&->eat(Int32)).parse("abc").should be Just.new({"1abc", ""})
      i1.flat_map(&->cancel(Int32)).parse("abc").should be Nothing({String, String}).new
      empty.flat_map(&->eat(Int32)).parse("abc").should be Nothing({String, String}).new
      empty.flat_map(&->cancel(Int32)).parse("abc").should be Nothing({String, String}).new
    end
  end

  describe "apply" do
    it "test Parser.apply" do
      psin = Parser.pure(->(x: Float64){ Math.sin(x) })
      pempty = Parser(Proc(Float64, Float64)).empty
      f1 = Parser.pure(1.0)
      fe = Parser(Float64).empty

      Parser.apply(psin){ f1 }.parse("abc").should be Just.new({Math.sin(1.0), "abc"})
      Parser.apply(psin){ fe }.parse("abc").should be Nothing({Float64, String}).new
      Parser.apply(pempty){ f1 }.parse("abc").should be Nothing({Float64, String}).new
      Parser.apply(pempty){ fe }.parse("abc").should be Nothing({Float64, String}).new
    end
  end

end
