import Testing
import Foundation

@testable import Parser

@Test func testCalculator(){
	let calc = Calculator()

	#expect(calc.calculate("72 - 7 - (1 - 2) * 3")! == (68.0, ""))
	#expect(calc.calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")! == (-8.889, ""))
	#expect(calc.calculate("3^(1+1)^3").map({ (round($0.0), $0.1) })! == (6561.0, ""))
	#expect(calc.calculate("sin(1+1)")! == (sin(2.0), ""))
	#expect(calc.calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")! == (-0.3634085731426532, ""))
	#expect(calc.calculate("sqr(2 + 3)")! == (25.0, ""))
	#expect(calc.calculate("sin(-PI/4)")! == (-0.7071067811865476, ""))
	#expect(calc.calculate(" E ^ PI")! == (23.140692632779267, ""))
	#expect(calc.calculate(" PI ^ E")! == (22.45915771836104, ""))
}
