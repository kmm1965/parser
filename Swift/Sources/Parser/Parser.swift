struct Parser<A> {
	nonisolated(unsafe) var parse: (String) -> Maybe<(A, String)>
	
	init(_ parse: @escaping (String) -> Maybe<(A, String)>){
		self.parse = parse
	}

	func map<B>(_ f: @escaping (A) -> B) -> Parser<B> {
		return Parser<B>({ [self] (inp) in self.parse(inp).map({ (f($0.0), $0.1) }) })
	}

	func flatMap<B>(_ f: @escaping (A) -> Parser<B>) -> Parser<B> {
		return Parser<B>({ [self] (inp) in self.parse(inp).flatMap({ f($0.0).parse($0.1) }) })
	}

    func skip<B>(_ fp: @escaping () -> Parser<B>) -> Parser<B> {
        return flatMap { [fp] (_) in fp() }
    }

    func orElse(_ f: @escaping () -> Parser<A>) -> Parser<A> {
        return Parser({ (inp) in self.parse(inp).orElse({ [f, inp] in f().parse(inp) }) })
    }

	static func pure(_ a: A) -> Parser<A> {
		return Parser<A>({ [a] in Maybe.Just((a, $0)) })
	}

	static func empty() -> Parser<A> {
		return Parser<A>({ (inp) in Maybe<(A, String)>.Nothing() })
	}

	static func apply<B>(_ pf: Parser<(A) -> B>, _ p: @escaping () -> Parser<A>) -> Parser<B> {
		return pf.flatMap({ [p] in p().map($0) })
	}
}
