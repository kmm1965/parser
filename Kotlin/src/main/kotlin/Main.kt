package org.example

import Calculator
import arrow.core.Some
import kotlin.math.round

fun main() {
    val calc = Calculator()

    println(calc.calculate("72 - 7 - (1 - 2) * 3"))                  // 68.0
    println(calc.calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")) // -8.889
    println(calc.calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"))      // -0.3634085731426532
    println(calc.calculate("sqr(sin(2)) + sqr(cos(1 + 1))"))         // 1.0
    println(calc.calculate("3 ^ 2 ^ 3").map { (x, s) -> Pair(round(x), s) }) // 6561.0
    println(calc.calculate("sin(- PI/4)"))                           // -0.7071067811865475
    println(calc.calculate(" E ^ PI "))                              // 23.140692632779267
    println(calc.calculate(" PI ^ E "))                              // 22.45915771836104
}
