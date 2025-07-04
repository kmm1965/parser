import Calculator.calculate

import scala.math.rint

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  println(calculate("72"))
  println(calculate("72 - 7 - (1 - 2) * 3"))
  println(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"))
  println(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"))
  println(calculate("sqr(sin(2)) + sqr(cos(1 + 1))"))
  println(calculate("3 ^ 2 ^ 3").map((x, s) => (rint(x), s)))
  println(calculate("sin(- PI/4)"))