#[cfg(test)]
mod tests {
    use crate::some_parsers::*;
    use crate::parser::*;

    #[test]
    fn test_any_char() {
        assert_eq!(any_char().parse("abc"), Some(('a', "bc".to_string())));
        assert_eq!(any_char().parse(""), None);
    }

    #[test]
    fn test_satisfy() {
        assert_eq!(satisfy(|c| c == 'a').parse("abc"), Some(('a', "bc".to_string())));
        assert_eq!(satisfy(|c| c == 'z').parse("abc"), None);
        assert_eq!(satisfy(|c| c == 'a').parse(""), None);
    }

    #[test]
    fn test_char() {
        assert_eq!(char_('a').parse("abc"), Some(('a', "bc".to_string())));
        assert_eq!(char_('z').parse("abc"), None);
        assert_eq!(char_('a').parse(""), None);
    }

    #[test]
    fn test_empty_string() {
        assert_eq!(empty_string().parse("abc"), Some(("".to_string(), "abc".to_string())));
    }

    #[test]
    fn test_spaces() {
        assert_eq!(spaces().parse("abc"), Some(("".to_string(), "abc".to_string())));
        assert_eq!(spaces().parse("   abc"), Some(("   ".to_string(), "abc".to_string())));
    }

    #[test]
    fn test_symbol() {
        assert_eq!(symbol('+').parse(" + abc"), Some(('+', "abc".to_string())));
        assert_eq!(symbol('+').parse("abc"), None);
    }

    #[test]
    fn test_alnum() {
        let a = alnum();
        assert_eq!(a.parse("123abc"), Some(('1', "23abc".to_string())));
        assert_eq!(a.parse("_123abc"), Some(('_', "123abc".to_string())));
        assert_eq!(a.parse("!@#"), None)
    }

    #[test]
    fn test_name() {
        let psin = name("sin");

        assert_eq!(psin.parse(" sin "), Some(("sin".to_string(), "".to_string())));
        assert_eq!(psin.parse(" sin (1.)"), Some(("sin".to_string(), "(1.)".to_string())));
        assert_eq!(psin.parse("sinus"), None);
    }

    #[test]
    fn test_optional() {
        assert_eq!(optional_c(char_('1')).parse("1234"), Some(("1".to_string(), "234".to_string())));
        assert_eq!(optional_c(char_('1')).parse("abc"), Some(("".to_string(), "abc".to_string())));
    }

    #[test]
    fn test_sign() {
        let s = sign();
        assert_eq!(s.parse("abc"), Some(("".to_string(), "abc".to_string())));
        assert_eq!(s.parse("+abc"), Some(("+".to_string(), "abc".to_string())));
        assert_eq!(s.parse("-abc"), Some(("-".to_string(), "abc".to_string())));

        let u = usign();
        assert_eq!(u.parse("abc"), Some(("".to_string(), "abc".to_string())));
        assert_eq!(u.parse(" + abc"), Some(("+".to_string(), "abc".to_string())));
        assert_eq!(u.parse(" - abc"), Some(("-".to_string(), "abc".to_string())));
    }

    #[test]
    fn test_digits() {
        let d = digits();
        assert_eq!(d.parse("123abc"), Some(("123".to_string(), "abc".to_string())));
        assert_eq!(d.parse("123  abc"), Some(("123".to_string(), "  abc".to_string())));
        assert_eq!(d.parse("abc"), Some(("".to_string(), "abc".to_string())));
    }

    #[test]
    fn test_double() {
        let d = double();
        assert_eq!(d.parse("1 abc"), Some((1.0, "abc".to_string())));
        assert_eq!(d.parse("1. abc"), Some((1.0, "abc".to_string())));
        assert_eq!(d.parse("1.23 abc"), Some((1.23, "abc".to_string())));
        assert_eq!(d.parse("-1.23 abc"), None);
        assert_eq!(d.parse(".23 abc"), Some((0.23, "abc".to_string())));
        assert_eq!(d.parse(" + 1.23 abc"), None);
        assert_eq!(d.parse("1.23e10abc"), Some((1.23e10, "abc".to_string())));
        assert_eq!(d.parse("1.23e-10abc"), Some((1.23e-10, "abc".to_string())));
        assert_eq!(d.parse("abc"), None);
    }

    #[test]
    fn test_between() {
        let expr = between(symbol('('), symbol(')'), double);

        assert_eq!(expr.parse(" ( 123 ) abc"), Some((123.0, "abc".to_string())));
        assert_eq!(expr.parse(" ( 123 abc"), None);
        assert_eq!(expr.parse(" 123 ) abc"), None);
    }

    #[test]
    fn test_chainlr1() {
        use std::rc::Rc;

        let fadd: Rc<dyn Fn(f64, f64) -> f64> = Rc::new(|x, y| x + y);
        let fsub: Rc<dyn Fn(f64, f64) -> f64> = Rc::new(|x, y| x - y);
        let fpow: Rc<dyn Fn(f64, f64) -> f64> = Rc::new(|x, y| (y * x.ln()).exp());
        let add = symbol('+').skip(|| Parser::pure(fadd.clone()));
        let sub = symbol('-').skip(|| Parser::pure(fsub.clone()));
        let pow = symbol('^').skip(|| Parser::pure(fpow.clone()));

        let pexpr = chainl1(double(), add.or_else_p(sub), false);

        assert_eq!(pexpr.parse("7abc"), Some((7.0, "abc".to_string())));
        assert_eq!(pexpr.parse(" 7 - 1 - 2 abc"), Some((4.0, "abc".to_string())));
        assert_eq!(pexpr.parse(" 7 - 1 + 2 - 3 abc"), Some((5.0, "abc".to_string())));
        assert_eq!(pexpr.parse("abc"), None);

        assert_eq!(chainr1(double(), pow.clone()).parse("3 ^ 2 ^ 3 abc").map(|pair| (pair.0.round(), pair.1)),
            Some((6561.0, "abc".to_string())));
    }
}