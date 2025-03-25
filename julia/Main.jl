include("Calc.jl")

using .MonadParser, .Calculator

import .MonadParser: my_parse
import .Calculator: expr

e = expr()

println("Hello from Main!")
println(my_parse(e, "72 - 7 - (1 - 2) * 3"))             # 68.0
println(my_parse(e, " 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")) # -8.889
println(my_parse(e, "3^(1+1)^3"))                        # 6561.0
println(my_parse(e, "sin(1+1)"))                         # 0.9092974268256817
println(my_parse(e, "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")) # -0.3634085731426532
println(my_parse(e, "sqr(2 + 3)"))                       # 25.0
println(my_parse(e, " E ^ PI"))                          # 23.140692632779263
println(my_parse(e, " PI ^ E"))                          # 22.459157718361038
