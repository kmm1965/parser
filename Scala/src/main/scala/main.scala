import Calculator.calculate

import scala.math.rint

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  println(calculate("72 - 7 - (1 - 2) * 3"))                  // 68.0
  println(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")) // -8.889
  println(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"))      // -0.3634085731426532
  println(calculate("sqr(sin(2)) + sqr(cos(1 + 1))"))         // 1.0
  println(calculate("3 ^ 2 ^ 3").map((x, s) => (rint(x), s))) // 6561.0
  println(calculate("sin(- PI/4)"))                           // -0.7071067811865475
  println(calculate(" E ^ PI "))                              // 23.140692632779267
  println(calculate(" PI ^ E "))                              // 22.45915771836104
