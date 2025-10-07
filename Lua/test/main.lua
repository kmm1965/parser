local calc = require("src/calculator")

print(calc.calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").value[1]) -- -8.889
print(calc.calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").value[1])      -- -0.36340857314265
print(calc.calculate("sqr(sin(2)) + sqr(cos(1 + 1))").value[1])         -- 1.0
print(calc.calculate("3 ^ 2 ^ 3").value[1])                             -- 6561.0
print(calc.calculate(" E ^ PI ").value[1])                              -- 23.140692632779
print(calc.calculate(" PI ^ E ").value[1])                              -- 22.459157718361
