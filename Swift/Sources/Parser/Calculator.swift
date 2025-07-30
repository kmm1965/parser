import Foundation

struct Calculator {
    static func op2(_ c: Character, _ f: @escaping (Double, Double) -> Double) -> Parser<(Double, Double) -> Double>{
        return symbol(c).skip({ [f] in Parser.pure(f) })
    }

    let add = op2("+", { (x, y) in x + y })
    let sub = op2("-", { (x, y) in x - y })
    let mul = op2("*", { (x, y) in x * y })
    let div = op2("/", { (x, y) in x / y })
    let pow = op2("^", { (x, y) in exp(y * log(x)) })

    static func fold<A>(_ parsers: [Parser<A>]) -> Parser<A>{
        var p0 = Parser<A>.empty()
        for p in parsers{
            p0 = p0.orElse({ () in p })
        }
        return token(p0)
    }

    static func defObject<A>(_ n: String, _ value: A) -> Parser<A>{
        return name(n).skip({ [value] in Parser.pure(value) })
    }

    let funcs = fold([
        defObject("sin", { (x: Double) in sin(x) }),
        defObject("cos", { (x: Double) in cos(x) }),
        defObject("asin", { (x: Double) in asin(x) }),
        defObject("acos", { (x: Double) in acos(x) }),
        defObject("sinh", { (x: Double) in sinh(x) }),
        defObject("cosh", { (x: Double) in cosh(x) }),
        defObject("tan", { (x: Double) in tan(x) }),
        defObject("log", { (x: Double) in log(x) }),
        defObject("log10", { (x: Double) in log10(x) }),
        defObject("exp", { (x: Double) in exp(x) }),
        defObject("sqrt", { (x: Double) in sqrt(x) }),
        defObject("sqr", { (x: Double) in x * x })
    ])

    let consts = fold([
        defObject("E", 		  2.7182818284590452), // e
        defObject("PI", 	  3.1415926535897932), // pi
        defObject("LOG2E",    1.4426950408889634), // log2(e)
        defObject("LOG10E",   0.4342944819032518), // log10(e)
        defObject("LN2", 	  0.6931471805599453), // ln(2)
        defObject("LN10", 	  2.302585092994046),  // ln(10)
        defObject("PI_2", 	  1.5707963267948966), // pi/2
        defObject("PI_4", 	  0.7853981633974483), // pi/4
        defObject("1_PI", 	  0.3183098861837907), // 1/pi
        defObject("2_PI", 	  0.6366197723675814), // 2/pi
        defObject("2_SQRTPI", 1.1283791670955126), // 2/sqrt(pi)
        defObject("SQRT2",    1.4142135623730951), // sqrt(2)
        defObject("SQRT1_2",  0.7071067811865476)  // 1/sqrt(2)
    ])

	func expr() -> Parser<Double>{
		return usign.flatMap({ (sgn) in chainl1(self.term(), self.add.orElse({ () in self.sub }), sgn == "-") })
	}

	func term() -> Parser<Double>{
		return chainl1(self.factor(), self.mul.orElse({ () in self.div }), false)
	}

	func factor() -> Parser<Double>{
		return chainr1(self.factor0(), self.pow)
	}

	func factor0() -> Parser<Double>{
		return self.expr_in_brackets()
			.orElse({ () in Parser.apply(self.funcs, { () in self.expr_in_brackets() }) })
			.orElse({ () in self.consts })
			.orElse({ () in double })
	}

	func expr_in_brackets() -> Parser<Double>{
		return between(symbol("("), symbol(")"), { [self] in self.expr() })
	}

	func calculate(_ s: String) -> Optional<(Double, String)>{
		return expr().parse(s).value
	}
}
