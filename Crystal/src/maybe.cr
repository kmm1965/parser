# Define the Maybe types
abstract class Maybe(T)
end

class Just(T) < Maybe(T)
  getter value : T

  def initialize(@value)
  end

  def same?(other : Maybe(T)) : Bool
    case other
    when Just(T)
      @value == other.value
    else
      false
    end
  end

  def map(&block : T -> U) : Maybe(U) forall U
    Just.new(yield @value)
  end

  def flat_map(&block : T -> Maybe(U)) : Maybe(U) forall U
    yield @value
  end

  def or_else(&block : -> Maybe(T)) : Maybe(T)
    self
  end
end

class Nothing(T) < Maybe(T)
  def initialize
  end

  def same?(other : Maybe(T)) : Bool
    case other
    when Just(T)
      false
    else
      true
    end
  end

  def map(&block : T -> U) : Maybe(U) forall U
    Nothing(U).new
  end

  def flat_map(&block : T -> Maybe(U)) : Maybe(U) forall U
    Nothing(U).new
  end

  def or_else(&block : -> Maybe(T)) : Maybe(T)
    yield
  end
end
