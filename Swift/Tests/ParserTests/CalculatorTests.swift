import Testing
import Foundation

@testable import Parser

@Test func testFuncs(){
	let calc = Calculator()

	#expect(calc.calculate("sin(2.0)")! == (sin(2.0), ""))
	#expect(calc.calculate("cos(2.0)")! == (cos(2.0), ""))
	#expect(calc.calculate("asin(0.5)")! == (asin(0.5), ""))
	#expect(calc.calculate("acos(0.5)")! == (acos(0.5), ""))
	#expect(calc.calculate("sinh(2.0)")! == (sinh(2.0), ""))
	#expect(calc.calculate("cosh(2.0)")! == (cosh(2.0), ""))
	#expect(calc.calculate("asinh(2.0)")! == (asinh(2.0), ""))
	#expect(calc.calculate("acosh(2.0)")! == (acosh(2.0), ""))
	#expect(calc.calculate("tan(2.0)")! == (tan(2.0), ""))
	#expect(calc.calculate("log(2.0)")! == (log(2.0), ""))
	#expect(calc.calculate("log10(2.0)")! == (log10(2.0), ""))
	#expect(calc.calculate("exp(2.0)")! == (exp(2.0), ""))
	#expect(calc.calculate("sqrt(2.0)")! == (sqrt(2.0), ""))
	#expect(calc.calculate("sqr(2.0)")! == (4.0, ""))
}

@Test func testConsts(){
	let calc = Calculator()
	let pi = 3.14159265358979323846

	#expect(calc.calculate("E")! == (2.7182818284590452, ""))
	#expect(calc.calculate("LOG2E")! == (1 / log(2.0), ""))
	#expect(calc.calculate("LOG10E")! == (0.4342944819032518, ""))
	//#expect(calc.calculate("LOG10E")! == (1 / log(10.0), ""))
	#expect(calc.calculate("LN2")! == (log(2.0), ""))
	#expect(calc.calculate("LN10")! == (log(10.0), ""))
	#expect(calc.calculate("PI")! == (pi, ""))
	#expect(calc.calculate("PI_2")! == (pi / 2, ""))
	#expect(calc.calculate("PI_4")! == (pi / 4, ""))
	#expect(calc.calculate("1_PI")! == (1 / pi, ""))
	#expect(calc.calculate("2_PI")! == (2 / pi, ""))
	#expect(calc.calculate("2_SQRTPI")! == (2 / sqrt(pi), ""))
	#expect(calc.calculate("SQRT2")! == (sqrt(2), ""))
	#expect(calc.calculate("SQRT1_2")! == (sqrt(0.5), ""))
}

@Test func testCalculator(){
	let calc = Calculator()

	#expect(calc.calculate("72 - 7 - (1 - 2) * 3")! == (68.0, ""))
	#expect(calc.calculate("-72 - 7 - (1 - 2) * 3")! == (-76.0, ""))
	#expect(calc.calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")! == (-8.889, ""))
	#expect(calc.calculate("3^(1+1)^3").map({ (round($0.0), $0.1) })! == (6561.0, ""))
	#expect(calc.calculate("sin(1+1)")! == (sin(2.0), ""))
	#expect(calc.calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")! == (-0.3634085731426532, ""))
	#expect(calc.calculate("sqr(2 + 3)")! == (25.0, ""))
	#expect(calc.calculate("sin(-PI/4)")! == (-0.7071067811865475, ""))
	#expect(calc.calculate(" E ^ PI")! == (23.140692632779267, ""))
	#expect(calc.calculate(" PI ^ E")! == (22.45915771836104, ""))
}
