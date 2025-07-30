import Testing
import Foundation

@testable import Parser

func safe_sqrt(_ x: Double) -> Maybe<Double>{
	return x >= 0 ? Maybe<Double>.Just(sqrt(x)) : Maybe<Double>.Nothing()
}

func safe_log(_ x: Double) -> Maybe<Double>{
	return x > 0 ? Maybe<Double>.Just(log(x)) : Maybe<Double>.Nothing()
}

@Test func testMaybeMap(){
	let toString: (Int) -> String = { String($0) }

	#expect(Maybe.Just(1.0).map(sin).value == Optional.some(sin(1.0)))
	#expect(Maybe<Double>.Nothing().map(sin).value == Optional.none)
	#expect(Maybe.Just(1).map(toString).value == Optional.some("1"))
	#expect(Maybe<Int>.Nothing().map(toString).value == Optional.none)
}

@Test func testMaybeFlatMap(){
	#expect(safe_sqrt(2.0).value == Optional.some(sqrt(2.0)))
	#expect(safe_sqrt(0.0).value == Optional.some(0.0))
	#expect(safe_sqrt(-2.0).value == Optional.none)

	#expect(safe_log(2.0).value == Optional.some(log(2.0)))
	#expect(safe_log(0.0).value == Optional.none)
	#expect(safe_log(-2.0).value == Optional.none)

	#expect(Maybe.Just(2.0).flatMap(safe_sqrt).value == Optional.some(sqrt(2.0)))
	#expect(Maybe.Just(0.0).flatMap(safe_sqrt).value == Optional.some(0.0))
	#expect(Maybe.Just(-2.0).flatMap(safe_sqrt).value == Optional.none)

	#expect(Maybe.Just(2.0).flatMap(safe_sqrt).flatMap(safe_log).value == Optional.some(log(sqrt(2.0))))
	#expect(Maybe.Just(0.0).flatMap(safe_sqrt).flatMap(safe_log).value == Optional.none)
	#expect(Maybe.Just(-2.0).flatMap(safe_sqrt).flatMap(safe_log).value == Optional.none)

	let toString: (Int) -> Maybe<String> = { $0 % 2 == 0 ? Maybe.Just(String($0)) : Maybe<String>.Nothing() }
	
	#expect(Maybe.Just(2).flatMap(toString).value == Optional.some("2"))
	#expect(Maybe.Just(1).flatMap(toString).value == Optional.none)
	#expect(Maybe<Int>.Nothing().flatMap(toString).value == Optional.none)
}
