require "spec"
require "../src/some_parsers"

describe SomeParsers do
  describe "empty_string" do
    it "test empty_string" do
      SomeParsers.empty_string().parse("abc").should be Just.new({"", "abc"})
    end
  end

  describe "anyChar" do
    it "test anyChar" do
	    SomeParsers.anyChar.parse("abc").should be Just.new({'a', "bc"})
	    SomeParsers.anyChar.parse("").should be Nothing({Char, String}).new
    end
  end

  describe "satisfy" do
    it "test satisfy" do
    	SomeParsers.satisfy{|c| c == 'a'}.parse("abc").should be Just.new({'a', "bc"})
    	SomeParsers.satisfy{|c| c == 'z'}.parse("abc").should be Nothing({Char, String}).new
    	SomeParsers.satisfy{|c| c == 'a'}.parse("").should be Nothing({Char, String}).new
    end
  end

  describe "char" do
    it "test char" do
      SomeParsers.char('a').parse("abc").should be Just.new({'a', "bc"})
      SomeParsers.char('z').parse("abc").should be Nothing({Char, String}).new
      SomeParsers.char('a').parse("").should be Nothing({Char, String}).new
    end
  end

  describe "spaces" do
    it "test spaces" do
      SomeParsers.spaces.parse("abc").should be Just.new({"", "abc"})
      SomeParsers.spaces.parse("   abc").should be Just.new({"   ", "abc"})
    end
  end

  describe "symbol" do
    it "test symbol" do
      SomeParsers.symbol('+').parse(" + abc").should be Just.new({'+', "abc"})
      SomeParsers.symbol('+').parse("abc").should be Nothing({Char, String}).new
    end
  end

  describe "alnum" do
    it "test alnum" do
      SomeParsers.alnum.parse("123abc").should be Just.new({'1', "23abc"})
      SomeParsers.alnum.parse("abc").should be Just.new({'a', "bc"})
      SomeParsers.alnum.parse("_123abc").should be Just.new({'_', "123abc"})
      SomeParsers.alnum.parse("!@#").should be Nothing({Char, String}).new
    end
  end

  describe "sign" do
    it "test sign" do
      SomeParsers.sign.parse("abc").should be Just.new({"", "abc"})
      SomeParsers.sign.parse("+abc").should be Just.new({"+", "abc"})
      SomeParsers.sign.parse("-abc").should be Just.new({"-", "abc"})

      SomeParsers.usign.parse("abc").should be Just.new({"", "abc"})
      SomeParsers.usign.parse(" + abc").should be Just.new({"+", "abc"})
      SomeParsers.usign.parse(" - abc").should be Just.new({"-", "abc"})
    end
  end

  describe "digits" do
    it "test digits" do
      SomeParsers.digits.parse("123abc").should be Just.new({"123", "abc"})
      SomeParsers.digits.parse("123  abc").should be Just.new({"123", "  abc"})
      SomeParsers.digits.parse("abc").should be Just.new({"", "abc"})
    end
  end

  describe "double" do
    it "test double" do
      SomeParsers.double.parse("1 abc").should be Just.new({1.0, "abc"})
      SomeParsers.double.parse("1. abc").should be Just.new({1.0, "abc"})
      SomeParsers.double.parse("1.23 abc").should be Just.new({1.23, "abc"})
      SomeParsers.double.parse("-1.23 abc").should be Nothing({Float64, String}).new
      SomeParsers.double.parse(".23 abc").should be Just.new({0.23, "abc"})
      SomeParsers.double.parse(" + 1.23 abc").should be Nothing({Float64, String}).new
      SomeParsers.double.parse("1.23e10abc").should be Just.new({1.23e10, "abc"})
      SomeParsers.double.parse("1.23e-10abc").should be Just.new({1.23e-10, "abc"})
      SomeParsers.double.parse("abc").should be Nothing({Float64, String}).new
    end
  end

  describe "between" do
    it "test between" do
      expr = SomeParsers.between(SomeParsers.symbol('('), SomeParsers.symbol(')')){ SomeParsers.double }

      expr.parse(" ( 123 ) abc").should be Just.new({123.0, "abc"})
      expr.parse(" ( 123 abc").should be Nothing({Float64, String}).new
      expr.parse(" 123 ) abc").should be Nothing({Float64, String}).new
    end
  end

  describe "chainlr1" do
    it "test chainlr1" do
      add = SomeParsers.symbol('+').skip{ Parser.pure(->(x: Float64, y: Float64){ x + y }) }
      sub = SomeParsers.symbol('-').skip{ Parser.pure(->(x: Float64, y: Float64){ x - y }) }
      pow = SomeParsers.symbol('^').skip{ Parser.pure(->(x: Float64, y: Float64){ Math.exp(y * Math.log(x)) }) }

      pexpr = SomeParsers.chainl1(SomeParsers.double, add.or_else{ sub }, false)

      pexpr.parse("7abc").should be Just.new({7.0, "abc"})
      pexpr.parse(" 7 - 1 - 2 abc").should be Just.new({4.0, "abc"})
      pexpr.parse(" 7 - 1 + 2 - 3 abc").should be Just.new({5.0, "abc"})
      pexpr.parse("abc").should be Nothing({Float64, String}).new

      SomeParsers.chainr1(SomeParsers.double, pow).parse("3 ^ 2 ^ 3 abc").map{ |p| {(p[0] + 0.5).to_i.to_f, p[1]} }.should be Just.new({6561.0, "abc"})
    end
  end

end
