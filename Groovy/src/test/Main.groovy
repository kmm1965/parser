package test

import main.Calculator

println(Calculator.calc("72 - 7 - (1 - 2) * 3"))                  // 68.0
println(Calculator.calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")) // -8.889
println(Calculator.calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"))      // -0.3634085731426532
println(Calculator.calc("sqr(sin(2)) + sqr(cos(1 + 1))"))         // 1.0
println(Calculator.calc("3 ^ 2 ^ 3").map { Tuple2<Double, String> pair -> new Tuple2(pair.getV1().round(), pair.getV2()) }) // 6561.0
println(Calculator.calc("sin(- PI/4)"))                           // -0.7071067811865475
println(Calculator.calc(" E ^ PI "))                              // 23.140692632779263
println(Calculator.calc(" PI ^ E "))                              // 22.45915771836104
