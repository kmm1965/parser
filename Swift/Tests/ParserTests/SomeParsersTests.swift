import Testing
import Foundation

@testable import Parser

@Test func testAnyChar(){
	#expect(anyChar.parse("abc").value! == ("a", "bc"))
	#expect(anyChar.parse("").value == nil)
}

@Test func testSatisfy(){
	#expect(satisfy({ $0 == "a" }).parse("abc").value! == ("a", "bc"))
	#expect(satisfy({ $0 == "z" }).parse("abc").value == nil)
	#expect(satisfy({ $0 == "a" }).parse("").value == nil)
}

@Test func testChar(){
	#expect(char("a").parse("abc").value! == ("a", "bc"))
	#expect(char("z").parse("abc").value == nil)
	#expect(char("a").parse("").value == nil)
}

@Test func testEmptyString(){
	#expect(empty_string.parse("abc").value! == ("", "abc"))
}

@Test func testOptional(){
	#expect(optional_c(char("1")).parse("1234").value! == ("1", "234"))
	#expect(optional_c(char("1")).parse("abc").value! == ("", "abc"))
}

@Test func testSpaces(){
	#expect(spaces.parse("abc").value! == ("", "abc"))
	#expect(spaces.parse("   abc").value! == ("   ", "abc"))
}

@Test func testSymbol(){
	#expect(symbol("+").parse(" + abc").value! == ("+", "abc"))
	#expect(symbol("+").parse("abc").value == nil)
}

@Test func testAlnum(){
	#expect(alnum.parse("123abc").value! == ("1", "23abc"))
	#expect(alnum.parse("abc").value! == ("a", "bc"))
	#expect(alnum.parse("_123abc").value! == ("_", "123abc"))
	#expect(alnum.parse("!@#").value == nil)
}

@Test func testName(){
	let psin = name("sin")

	#expect(psin.parse(" sin ").value! == ("sin", ""))
	#expect(psin.parse(" sin (1.)").value! == ("sin", "(1.)"))
	#expect(psin.parse("sinus").value == nil)
}

@Test func testSign(){
	#expect(sign.parse("abc").value! == ("", "abc"))
	#expect(sign.parse("+abc").value! == ("+", "abc"))
	#expect(sign.parse("-abc").value! == ("-", "abc"))

	#expect(usign.parse("abc").value! == ("", "abc"))
	#expect(usign.parse(" + abc").value! == ("+", "abc"))
	#expect(usign.parse(" - abc").value! == ("-", "abc"))
}

@Test func testDigits(){
	#expect(digits.parse("123abc").value! == ("123", "abc"))
	#expect(digits.parse("123  abc").value! == ("123", "  abc"))
	#expect(digits.parse("abc").value! == ("", "abc"))
}

@Test func testDouble(){
	#expect(double.parse("1 abc").value! == (1.0, "abc"))
	#expect(double.parse("1. abc").value! == (1.0, "abc"))
	#expect(double.parse("1.23 abc").value! == (1.23, "abc"))
	#expect(double.parse("-1.23 abc").value == nil) // (-1.23, "abc"))
	#expect(double.parse(".23 abc").value! == (0.23, "abc"))
	#expect(double.parse(" + 1.23 abc").value == nil)
	#expect(double.parse("1.23e10abc").value! == (1.23e10, "abc"))
	#expect(double.parse("1.23e-10abc").value! == (1.23e-10, "abc"))
	#expect(double.parse("abc").value == nil)
}

@Test func testBetween(){
	let expr = between(symbol("("), symbol(")"), { () in double })

	#expect(expr.parse(" ( 123 ) abc").value! == (123.0, "abc"))
	#expect(expr.parse(" ( 123 abc").value == nil)
	#expect(expr.parse(" 123 ) abc").value == nil)
}

@Test func testChainlr1(){
	let add = symbol("+").skip({ Parser.pure({ (x: Double, y: Double) in x + y }) })
	let sub = symbol("-").skip({ Parser.pure({ (x: Double, y: Double) in x - y }) })
	let pow = symbol("^").skip({ Parser.pure({ (x: Double, y: Double) in exp(y * log(x)) }) })

	let pexpr = chainl1(double, add.orElse { () in sub }, false)

	#expect(pexpr.parse("7abc").value! == (7.0, "abc"))
	#expect(pexpr.parse(" 7 - 1 - 2 abc").value! == (4.0, "abc"))
	#expect(pexpr.parse(" 7 - 1 + 2 - 3 abc").value! == (5.0, "abc"))
	#expect(pexpr.parse("abc").value == nil)

	#expect(chainr1(double, pow).parse("3 ^ 2 ^ 3 abc").map({ (round($0.0), $0.1) }).value! == (6561.0, "abc"))
}
