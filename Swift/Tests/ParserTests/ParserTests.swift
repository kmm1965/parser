import Testing
import Foundation

@testable import Parser

@Test func testParserPure(){
	#expect(Parser.pure(1).parse("abc").value! == (1, "abc"))
	#expect(Parser.pure(1.0).parse("abc").value! == (1.0, "abc"))
	#expect(Parser.pure("1").parse("abc").value! == ("1", "abc"))
}

@Test func testParserMap(){
	let fi = { (i: Int) in String(i) }
	let fd = { (d: Double) in String(d) }
	let fs = { (s: String) in Int(s)! }
	
	#expect(Parser.pure(1).map(fi).parse("abc").value! == ("1", "abc"))
	#expect(Parser.pure(1.0).map(fd).parse("abc").value! == ("1.0", "abc"))
	#expect(Parser.pure("1").map(fs).parse("abc").value! == (1, "abc"))
	
	#expect(Parser<Int>.empty().map(fi).parse("abc").value == nil)
	#expect(Parser<Double>.empty().map(fd).parse("abc").value == nil)
	#expect(Parser<String>.empty().map(fs).parse("abc").value == nil)
}

@Test func testParserApply(){
	let psin = Parser.pure({ (x: Double) in sin(x) })
	let emptyf = Parser<(Double) -> Double>.empty()
	let fd = { () in Parser.pure(1.0) }
	let nf = { () in Parser<Double>.empty() }

	#expect(Parser.apply(psin, fd).parse("abc").value! == (sin(1.0), "abc"))
	#expect(Parser.apply(psin, nf).parse("abc").value == nil)
	#expect(Parser.apply(emptyf, fd).parse("abc").value == nil)
	#expect(Parser.apply(emptyf, nf).parse("abc").value == nil)
}

@Test func testParserFlatMap(){
	let i1 = Parser.pure(1)
	let iempty = Parser<Int>.empty()
	let eat = { (x: Int) in Parser({ Maybe.Just((String(x) + $0, "")) }) }
	let cancel = { (x: Int) in Parser { (_) in Maybe<(String, String)>.Nothing() } }

	#expect(i1.flatMap(eat).parse("abc").value! == ("1abc", ""))
	#expect(i1.flatMap(cancel).parse("abc").value == nil)
	#expect(iempty.flatMap(eat).parse("abc").value == nil)
	#expect(iempty.flatMap(cancel).parse("abc").value == nil)
}
