use std::rc::Rc;

use crate::some_parsers::*;
use crate::parser::*;

pub struct Calculator<'a> {
    add: Parser<'a, Rc<dyn Fn(f64, f64) -> f64 + 'a>>,
    sub: Parser<'a, Rc<dyn Fn(f64, f64) -> f64 + 'a>>,
    mul: Parser<'a, Rc<dyn Fn(f64, f64) -> f64 + 'a>>,
    div: Parser<'a, Rc<dyn Fn(f64, f64) -> f64 + 'a>>,
    pow: Parser<'a, Rc<dyn Fn(f64, f64) -> f64 + 'a>>,

    funcs: Parser<'a, Rc<dyn Fn(f64) -> f64 + 'a>>,
    consts: Parser<'a, f64>,
}

impl<'a> Clone for Calculator<'a> {
    fn clone(&self) -> Self {
        Self {
            add: self.add.clone(),
            sub: self.sub.clone(),
            mul: self.mul.clone(),
            div: self.div.clone(),
            pow: self.pow.clone(),
            funcs: self.funcs.clone(),
            consts: self.consts.clone()
        }
    }
}

impl <'a> Calculator<'a> {
    pub fn new() -> Self {
        Self {
            add: symbol('+').skip(|| Parser::pure(Rc::new(|x, y| x + y) as Rc<dyn Fn(f64, f64) -> f64>)),
            sub: symbol('-').skip(|| Parser::pure(Rc::new(|x, y| x - y) as Rc<dyn Fn(f64, f64) -> f64>)),
            mul: symbol('*').skip(|| Parser::pure(Rc::new(|x, y| x * y) as Rc<dyn Fn(f64, f64) -> f64>)),
            div: symbol('/').skip(|| Parser::pure(Rc::new(|x, y| x / y) as Rc<dyn Fn(f64, f64) -> f64>)),
            pow: symbol('^').skip(|| Parser::pure(Rc::new(|x: f64, y: f64| x.powf(y)) as Rc<dyn Fn(f64, f64) -> f64>)),

            funcs: identifier().and_then(move |n| [
                Self::guard(n == "sin",   Rc::new(|x: f64| x.sin()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "cos",   Rc::new(|x: f64| x.cos()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "asin",  Rc::new(|x: f64| x.asin()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "acos",  Rc::new(|x: f64| x.acos()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "sinh",  Rc::new(|x: f64| x.sinh()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "cosh",  Rc::new(|x: f64| x.cosh()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "tan",   Rc::new(|x: f64| x.tan()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "log",   Rc::new(|x: f64| x.ln()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "log10", Rc::new(|x: f64| x.log10()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "exp",   Rc::new(|x: f64| x.exp()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "sqrt",  Rc::new(|x: f64| x.sqrt()) as Rc<dyn Fn(f64) -> f64>),
                Self::guard(n == "sqr",   Rc::new(Self::sqr) as Rc<dyn Fn(f64) -> f64>),
            ].iter().fold(Parser::empty(), |p0, p| p0.or_else_p(p.clone()))),

            consts: identifier().and_then(move |n| [
                Self::guard(n == "E",        std::f64::consts::E),
                Self::guard(n == "PI",       std::f64::consts::PI),
                Self::guard(n == "LOG2E",    1.44269504088896340736),  // log2(e)
                Self::guard(n == "LOG10E",   0.434294481903251827651), // log10(e)
                Self::guard(n == "LN2",      0.693147180559945309417), // ln(2)
                Self::guard(n == "LN10",     2.30258509299404568402),  // ln(10)
                Self::guard(n == "PI_2",     1.57079632679489661923),  // pi/2
                Self::guard(n == "PI_4",     0.785398163397448309616), // pi/4
                Self::guard(n == "1_PI",     0.318309886183790671538), // 1/pi
                Self::guard(n == "2_PI",     0.636619772367581343076), // 2/pi
                Self::guard(n == "2_SQRTPI", 1.12837916709551257390),  // 2/sqrt(pi)
                Self::guard(n == "SQRT2",    1.41421356237309504880),  // sqrt(2)
                Self::guard(n == "SQRT1_2",  0.707106781186547524401)  // 1/sqrt(2)
            ].iter().fold(Parser::empty(), |p0, p| p0.or_else_p(p.clone()))),
        }
    }

    fn sqr(x: f64) -> f64 { return x * x; }

    fn guard<A: Clone + 'a>(b: bool, value: A) -> Parser<'a, A> {
        if b { Parser::pure(value.clone()) } else { Parser::empty() }
    }

    fn expr_in_brackets(&self) -> Parser<'a, f64> {
        between(symbol('('), symbol(')'), {
            let calc = self.clone();
            move || calc.clone().expr()
        })
    }

    fn factor0(&self) -> Parser<'a, f64> {
        let calc = self.clone();
        let calc2 = self.clone();
        self.expr_in_brackets()
            .or_else(move || Parser::apply(calc.funcs.clone(), {
                let calc_ = calc.clone();
                move || calc_.clone().expr_in_brackets()
            })).or_else(move || calc2.clone().consts)
            .or_else(|| double())
    }

    fn factor(&self) -> Parser<'a, f64> {
        let calc = self.clone();
        chainr1(self.factor0(), calc.pow.clone())
    }

    fn term(&self) -> Parser<'a, f64> {
        let calc = self.clone();
        chainl1(self.factor(), calc.clone().mul.or_else_p(calc.clone().div), false)
    }

    pub fn expr(&self) -> Parser<'a, f64> {
        usign().and_then({
            let calc = self.clone();
            move |sgn| chainl1(calc.clone().term(), calc.clone().add.or_else_p(calc.clone().sub), sgn == "-")
        })
    }

    pub fn calc(&self, inp: &str) -> Option<(f64, String)> {
        self.expr().parse(inp)
    }
}
