require "spec"
require "../src/maybe"

def safe_sqrt(x : Float64) : Maybe(Float64)
  x >= 0 ? Just.new(Math.sqrt(x)) : Nothing(Float64).new
end

def safe_log(x : Float64) : Maybe(Float64)
  x > 0 ? Just.new(Math.log(x)) : Nothing(Float64).new
end

def toString(i : Int32) : Maybe(String)
  i % 2 == 0 ? Just.new(i.to_s) : Nothing(String).new
end

describe Maybe do
  describe "fmap" do
    it "test Maybe.fmap" do
      Just.new(1.0).map(&->Math.sin(Float64)).should be Just.new(Math.sin(1.0))
      Nothing(Float64).new.map(&->Math.sin(Float64)).should be Nothing(Float64).new
      Just.new(1).map(&.to_s).should be Just.new("1")
      Nothing(Int32).new.map(&.to_s).should be Nothing(String).new
    end
  end

  describe "flat_map" do
    it "test Maybe.flat_map" do
      safe_sqrt(2.0).should be Just.new(Math.sqrt(2.0))
      safe_sqrt(0.0).should be Just.new(0.0)
      safe_sqrt(-2.0).should be Nothing(Float64).new

      safe_log(2.0).should be Just.new(Math.log(2.0))
      safe_log(0.0).should be Nothing(Float64).new
      safe_log(-2.0).should be Nothing(Float64).new

      Just.new(2.0).flat_map(&->safe_sqrt(Float64)).should be Just.new(Math.sqrt(2.0))
      Just.new(0.0).flat_map(&->safe_sqrt(Float64)).should be Just.new(0.0)
      Just.new(-2.0).flat_map(&->safe_sqrt(Float64)).should be Nothing(Float64).new

      Just.new(2.0).flat_map(&->safe_sqrt(Float64)).flat_map(&->safe_log(Float64)).should be Just.new(Math.log(Math.sqrt(2.0)))
      Just.new(0.0).flat_map(&->safe_sqrt(Float64)).flat_map(&->safe_log(Float64)).should be Nothing(Float64).new
      Just.new(-2.0).flat_map(&->safe_sqrt(Float64)).flat_map(&->safe_log(Float64)).should be Nothing(Float64).new

      Just.new(2).flat_map(&->toString(Int32)).should be Just.new("2")
      Just.new(1).flat_map(&->toString(Int32)).should be Nothing(String).new
      Nothing(Int32).new.flat_map(&->toString(Int32)).should be Nothing(String).new
    end
  end

  describe "or_else" do
    it "test Maybe.or_else" do
      Just.new(1.0).or_else{Just.new(2.0)}.should be Just.new(1.0)
      Nothing(Float64).new.or_else{Just.new(2.0)}.should be Just.new(2.0)
    end
  end
end