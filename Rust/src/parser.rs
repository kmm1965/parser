use std::rc::Rc;

pub struct Parser<'a, A> {
    unp: Rc<dyn Fn(&str) -> Option<(A, String)> + 'a>
}

impl<'a, A> Clone for Parser<'a, A> {
    fn clone(&self) -> Self {
        Self { unp: self.unp.clone() }
    }
}

impl <'a, A: 'a> Parser<'a, A> {
    pub fn new(unp: impl Fn(&str) -> Option<(A, String)> + 'a) -> Self {
        Self { unp: Rc::new(unp) }
    }

    pub fn parse(&self, inp: &str) -> Option<(A, String)> {
        (self.unp)(inp)
    }

    pub fn map<B: 'a, F>(self, f: F) -> Parser<'a, B>
        where F: Fn(A) -> B + 'a
    {
        Parser::new(move |inp| (self.unp)(inp).map(|(x, out)| (f(x), out)))
    }

    pub fn map_rc<B: 'a, F>(self, f: Rc<F>) -> Parser<'a, B>
        where F: Fn(A) -> B + 'a + ?Sized
    {
        Parser::new(move |inp| (self.unp)(inp).map(|(x, out)| (f(x), out)))
    }

    pub fn and_then<B: 'a, F>(self, f: F) -> Parser<'a, B>
        where F: Fn(A) -> Parser<'a, B> + 'a
    {
        Parser::new(move |inp| (self.unp)(inp).and_then(|(x, out)| f(x).parse(out.as_str())))
    }

    pub fn skip<B: 'a, FP>(self, fp: FP) -> Parser<'a, B>
        where FP: Fn() -> Parser<'a, B> + 'a
    {
        self.and_then(move |_| fp())
    }

    pub fn skip_p<B: 'a>(self, p: Parser<'a, B>) -> Parser<'a, B> {
        self.skip(move || p.clone())
    }

    pub fn apply<B: 'a, PF, FP>(pf: Parser<'a, Rc<PF>>, fp: FP) -> Parser<'a, B>
        where
            PF: Fn(A) -> B + 'a + ?Sized,
            FP: Fn() -> Parser<'a, A> + 'a
    {
        pf.and_then(move |f| fp().map_rc(f))
    }

    pub fn apply_p<B: 'a, PF>(pf: Parser<'a, Rc<PF>>, p: Parser<'a, A>) -> Parser<'a, B>
        where PF: Fn(A) -> B + 'a + ?Sized
    {
        Self::apply(pf, move || p.clone())
    }

    pub fn or_else(self, f: impl Fn() -> Parser<'a, A> + 'a) -> Parser<'a, A> {
        Parser::new(move |inp| (self.unp)(inp).or_else(|| f().parse(inp)))
    }

    pub fn or_else_p(self, p: Parser<'a, A>) -> Parser<'a, A> {
        self.or_else(move || p.clone())
    }

    pub fn empty() -> Parser<'a, A> {
        Parser::new(|_| None)
    }
}

impl<'a, A: Clone + 'a> Parser<'a, A> {
    pub fn pure(a: A) -> Parser<'a, A> {
        Parser::new(move |inp| Some((a.clone(), inp.to_string())))
    }
}

pub fn some<'a>(p: Parser<'a, char>) -> Parser<'a, String> {
    Parser::apply(p.clone().map(move |c| Rc::new(move |s: String| c.to_string() + &s) as Rc<dyn Fn(String) -> String + 'a>),
        move || many(p.clone()))
}

pub fn many<'a>(p: Parser<'a, char>) -> Parser<'a, String> {
    some(p).or_else(move || Parser::pure("".to_string()))
}
