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

    static func guard_<A>(_ b: Bool, _ value: A) -> Parser<A>{
        return b ? Parser.pure(value) : Parser<A>.empty()
    }

    let funcs = identifier.flatMap({ (n) in fold([
        guard_(n == "sin",   { (x: Double) in sin(x) }),
        guard_(n == "cos",   { (x: Double) in cos(x) }),
        guard_(n == "asin",  { (x: Double) in asin(x) }),
        guard_(n == "acos",  { (x: Double) in acos(x) }),
        guard_(n == "sinh",  { (x: Double) in sinh(x) }),
        guard_(n == "cosh",  { (x: Double) in cosh(x) }),
        guard_(n == "asinh", { (x: Double) in asinh(x) }),
        guard_(n == "acosh", { (x: Double) in acosh(x) }),
        guard_(n == "tan",   { (x: Double) in tan(x) }),
        guard_(n == "log",   { (x: Double) in log(x) }),
        guard_(n == "log10", { (x: Double) in log10(x) }),
        guard_(n == "exp",   { (x: Double) in exp(x) }),
        guard_(n == "sqrt",  { (x: Double) in sqrt(x) }),
        guard_(n == "sqr",   { (x: Double) in x * x })
    ]) })
    
    let consts = identifier.flatMap({ (n) in fold([
        guard_(n == "E",        2.718281828459045235360),
        guard_(n == "PI",       3.141592653589793238462),
        guard_(n == "LOG2E",    1.44269504088896340736),  // log2(e)
        guard_(n == "LOG10E",   0.434294481903251827651), // log10(e)
        guard_(n == "LN2",      0.693147180559945309417), // ln(2)
        guard_(n == "LN10",     2.30258509299404568402),  // ln(10)
        guard_(n == "PI_2",     1.57079632679489661923),  // pi/2
        guard_(n == "PI_4",     0.785398163397448309616), // pi/4
        guard_(n == "1_PI",     0.318309886183790671538), // 1/pi
        guard_(n == "2_PI",     0.636619772367581343076), // 2/pi
        guard_(n == "2_SQRTPI", 1.12837916709551257390),  // 2/sqrt(pi)
        guard_(n == "SQRT2",    1.41421356237309504880),  // sqrt(2)
        guard_(n == "SQRT1_2",  0.707106781186547524401)  // 1/sqrt(2)
      ]) })
    
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
