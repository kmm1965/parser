require "../src/calculator";

def maybeToString(m : Maybe({Float64, String})) : String
  case m
  when Just({Float64, String})
    "(" + m.value[0].to_s + "," + m.value[1] + ")"
  else
    "Nothing"
  end
end

print(maybeToString(Calculator.calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")) + "\n") # -8.889
print(maybeToString(Calculator.calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")) + "\n")      # -0.3634085731426532
print(maybeToString(Calculator.calculate("sqr(sin(2)) + sqr(cos(1 + 1))")) + "\n")         # 1.0
print(maybeToString(Calculator.calculate("3 ^ 2 ^ 3")) + "\n")                             # 6561.0
print(maybeToString(Calculator.calculate(" E ^ PI ")) + "\n")                              # 23.140692632779267
print(maybeToString(Calculator.calculate(" PI ^ E ")) + "\n")                              # 22.45915771836104

