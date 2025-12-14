use std::rc::Rc;

use crate::parser::*;

pub fn any_char<'a>() -> Parser<'a, char> {
    Parser::new(move |inp| if inp.len() > 0 { Some((inp.chars().nth(0).unwrap(), inp[1..].to_string())) } else { None })
}

pub fn satisfy<'a, P>(pred: P) -> Parser<'a, char> 
    where P: Fn(char) -> bool + 'a
{
    any_char().and_then(move |c| if pred(c) { Parser::pure(c) } else { Parser::empty() })
}

pub fn char_<'a>(c: char) -> Parser<'a, char> {
    satisfy(move |c1| c1 == c)
}

pub fn empty_string<'a>() -> Parser<'a, String> {
    Parser::pure("".to_string())
}

pub fn between<'a, O: 'a, C: 'a, A: Clone + 'a, P>(open: Parser<'a, O>, close: Parser<'a, C>, p: P) -> Parser<'a, A>
    where P: Fn() -> Parser<'a, A> + 'a
{
    open.skip(move || p().and_then({
        let close2 = close.clone();
        move |a| <Parser<'_, C> as Clone>::clone(&close2).skip(move || Parser::pure(a.clone()))
    }))
}

pub fn spaces<'a>() -> Parser<'a, String> {
    many(satisfy(|c| c.is_whitespace()))
}

pub fn token<'a, A: Clone + 'a>(p: Parser<'a, A>) -> Parser<'a, A> {
    between(spaces(), spaces(), move || p.clone())
}

pub fn symbol<'a>(c: char) -> Parser<'a, char>{
    token(char_(c))
}

pub fn alnum<'a>() -> Parser<'a, char> {
    satisfy(|c| c.is_alphanumeric() || c == '_')
}

pub fn identifier<'a>() -> Parser<'a, String> {
    token(some(alnum()))
}

pub fn name<'a>(n: &'a str) -> Parser<'a, String> {
    identifier().and_then(move |s| if s == n.to_string() { Parser::pure(n.to_string()) } else { Parser::empty() })
}

pub fn optional_s<'a>(p: Parser<'a, String>) -> Parser<'a, String> {
    p.or_else(empty_string)
}

pub fn optional_c<'a>(p: Parser<'a, char>) -> Parser<'a, String> {
    optional_s(p.map(|c| c.to_string()))
}

pub fn digit<'a>() -> Parser<'a, char> {
    satisfy(|c| c.is_digit(10))
}

pub fn digits<'a>() -> Parser<'a, String> {
    many(digit())
}

pub fn sign<'a>() -> Parser<'a, String> {
    optional_c(char_('+').or_else(|| char_('-')))
}

// Unary sign
pub fn usign<'a>() -> Parser<'a, String>{
    token(sign())
}

pub fn  double<'a>() -> Parser<'a, f64> {
    token(digits().and_then(
        move |int_part| optional_s(char_('.').skip(digits)).and_then(
        move |frac_part| optional_s(char_('e').or_else(|| char_('E')).skip(sign).and_then(
            move |exp_sign| some(digit()).and_then(
            move |exp_digits| Parser::pure(exp_sign.clone() + &exp_digits)))).and_then(
        {
            let int_part2 = int_part.clone();
            move |exp_part|
                if int_part2.len() > 0 || frac_part.len() > 0 {
                    let fp = if frac_part.len() > 0 { String::from(".") + &frac_part } else { String::from("") };
                    let ep = if exp_part.len()  > 0 { String::from("e") + &exp_part  } else { String::from("") };
                    Parser::pure((int_part2.clone() + &fp + &ep).parse::<f64>().unwrap())
                } else { Parser::empty() }
        }))))
}

fn rest<'a, FP: 'a, FF: Clone + 'a, OP>(fp: FP, ff: FF, op: Parser<'a, Rc<OP>>, a: f64) -> Parser<'a, f64>
    where
        FP: Fn() -> Parser<'a, f64> + 'a,
        FF: Fn(f64) -> Parser<'a, f64> + 'a,
        OP: Fn(f64, f64) -> f64 + 'a + ?Sized
{
    op.and_then(move |f| {
        let ff2 = ff.clone();
        fp().and_then(move |b| ff2(f(a, b)))
    }).or_else(move ||Parser::pure(a))
}

fn rest_l<'a, OP>(p: Parser<'a, f64>, op: Parser<'a, Rc<OP>>, a: f64) -> Parser<'a, f64>
    where OP: Fn(f64, f64) -> f64 + 'a + ?Sized
{
    let p2 = p.clone();
    let p3 = p.clone();
    let op2 = op.clone();
    let op3 = op.clone();
    return rest(move || p2.clone(), move |b| rest_l(p3.clone(), op2.clone(), b), op3.clone(), a);
}

fn rest_r<'a, OP>(p: Parser<'a, f64>, op: Parser<'a, Rc<OP>>, a: f64) -> Parser<'a, f64>
    where OP: Fn(f64, f64) -> f64 + 'a + ?Sized
{
    let op2 = op.clone();
    let op3 = op.clone();
    rest(move || chainr1(p.clone(), op2.clone()), Parser::pure, op3.clone(), a)
}

pub fn chainl1<'a, OP>(p: Parser<'a, f64>, op: Parser<'a, Rc<OP>>, negate_first: bool) -> Parser<'a, f64>
    where OP: Fn(f64, f64) -> f64 + 'a + ?Sized
{
    p.clone().and_then(move |a| rest_l(p.clone(), op.clone(), if negate_first { -a } else { a } ))
}

pub fn chainr1<'a, OP>(p: Parser<'a, f64>, op: Parser<'a, Rc<OP>>) -> Parser<'a, f64>
    where OP: Fn(f64, f64) -> f64 + 'a + ?Sized
{
    p.clone().and_then(move |a| rest_r(p.clone(), op.clone(), a))
}
