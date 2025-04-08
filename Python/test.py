from calculator import Calculator

expr = Calculator().expr()

print(expr.parse("72 - 7 - (1 - 2) * 3").value)                  # 68.0
print(expr.parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").value) # -8.889
print(expr.parse("3 ^ (1 + 1) ^ 3").value)                       # 6561.0
print(expr.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").value)      # -0.3634085731426532
print(expr.parse("sqr(sin(2)) + sqr(cos(1 + 1))").value)         # 1.0
print(expr.parse("E ^ PI").value)                                # 23.140692632779263
print(expr.parse("PI ^ E").value)                                # 22.45915771836104
print(expr.parse("sin(-PI/4)").value)                            # -0.7071067811865476
