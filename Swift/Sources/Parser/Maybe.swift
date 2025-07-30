struct Maybe<A> {
	var value: Optional<A>
	
	init(){
		self.value = Optional<A>.none
	}
	
	init(_ value: A){
		self.value = value
	}
	
	static func Nothing() -> Maybe<A>{
		return Maybe<A>()
	}
	
	static func Just(_ value: A) -> Maybe<A>{
		return Maybe<A>(value)
	}

	func map<B>(_ f: (A) -> B) -> Maybe<B> {
		if let v: A = value {
			return Maybe<B>.Just(f(v))
		} else { return Maybe<B>.Nothing() }
	}

	func flatMap<B>(_ f: (A) -> Maybe<B>) -> Maybe<B>{
		if let v: A = value {
			return f(v)
		} else { return Maybe<B>.Nothing() }
	}

	func orElse(_ f: () -> Maybe<A>) -> Maybe<A>{
		if let _: A = value {
			return self
		} else { return f() }
	}
}
