from calculator import Calculator

expr = Calculator().expr()

print(expr.parse("72 - 7 - (1 - 2) * 3").value)             # 68.0
print(expr.parse("3 ^ (1 + 1) ^ 3").value)                  # 6561.0
print(expr.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").value) # -0.3634085731426532
print(expr.parse("sqr(sin(2)) + sqr(cos(1 + 1))").value)    # 1.0

