let anyChar: Parser<Character> = Parser({ (inp) in inp.isEmpty ?
	Maybe<(Character, String)>.Nothing() :
	Maybe.Just((Character(String(inp.first!)), String(inp[inp.index(inp.startIndex, offsetBy: 1)..<inp.endIndex])))
})

func satisfy(_ pred: @escaping (Character) -> Bool) -> Parser<Character>{
	return anyChar.flatMap({ [pred] (c) in pred(c) ? Parser.pure(c) : Parser<Character>.empty() })
}

func char(_ c: Character) -> Parser<Character>{
	return satisfy({ [c] in $0 == c })
}

let empty_string = Parser.pure("")

func between<A, O, C>(_ open: Parser<O>, _ close: Parser<C>, _ p: @escaping () -> Parser<A>) -> Parser<A>{
	return open.flatMap({ [p] (_) in p().flatMap({ (x) in close.flatMap({ (_) in Parser.pure(x) }) }) })
}

func some(_ p: Parser<Character>) -> Parser<String>{
	return Parser.apply(p.map{ (c) in { (s: String) in String(c) + s } }, { [p] in many(p) })
}

func many(_ p: Parser<Character>) -> Parser<String>{
	return some(p).orElse({ () in empty_string })
}

let spaces = many(satisfy({ $0.isWhitespace }))

func token<A>(_ p: Parser<A>) -> Parser<A>{
	return between(spaces, spaces, { [p] in p })
}

func symbol(_ c: Character) -> Parser<Character>{
	return token(char(c))
}

let alnum = satisfy({ (c) in c.isLetter || c.isNumber || c == "_" })

func name(_ n: String) -> Parser<String>{
	return token(some(alnum).flatMap({ $0 == n ? Parser.pure(n) : Parser<String>.empty() }))
}

func optional_s(_ p: Parser<String>) -> Parser<String>{
	return p.orElse({ () in empty_string })
}

func optional_c(_ p: Parser<Character>) -> Parser<String>{
	return optional_s(p.map({ String($0) }))
}

let digit = satisfy({ $0.isNumber })

let digits = many(digit)

let sign = optional_c(char("+").orElse({ () in char("-") }))

let usign = optional_c(symbol("+").orElse({ () in symbol("-") }))

let double: Parser<Double> = token(digits.flatMap({
	(int_part) in optional_s(char(".").skip({ digits })).flatMap({
	(frac_part) in optional_s(char("e").orElse({ char("E") }).skip({ () in sign }).flatMap({
		(exp_sign) in some(digit).flatMap({
		(exp_digits) in Parser.pure(exp_sign + exp_digits) }) })).flatMap({
	(exp_part) in !int_part.isEmpty || !frac_part.isEmpty ?
		Parser.pure(Double(int_part +
			(!frac_part.isEmpty ? "." + frac_part : "") +
			(!exp_part.isEmpty ? "e" + exp_part : "" ))!)
		: Parser.empty()}) }) }))

func rest<A>(_ p: @escaping () -> Parser<A>, _ ff: @escaping (A) -> Parser<A>, _ op: Parser<(A, A) -> A>, _ a: A) -> Parser<A>{
	return op.flatMap({ [p] (f) in p().flatMap { [ff, f] (b) in ff(f(a, b)) } }).orElse({ () in Parser.pure(a) })
}

func rest_l<A>(_ p: Parser<A>, _ op: Parser<(A, A) -> A>, _ a: A) -> Parser<A>{
	return rest({ () in p }, { (b) in rest_l(p, op,  b) }, op, a)
}

func rest_r<A>(_ p: Parser<A>, _ op: Parser<(A, A) -> A>, _ a: A) -> Parser<A>{
	return rest({ () in chainr1(p, op) }, { (b) in Parser.pure(b) }, op, a)
}

func chainl1(_ p: Parser<Double>, _ op: Parser<(Double, Double) -> Double>, _ negate_first: Bool) -> Parser<Double>{
	return p.flatMap({ (a) in rest_l(p, op, negate_first ? -a : a) })
}

func chainr1<A>(_ p: Parser<A>, _ op: Parser<(A, A) -> A>) -> Parser<A>{
	return p.flatMap({ (a) in rest_r(p, op, a) })
}
