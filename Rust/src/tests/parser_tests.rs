#[cfg(test)]
mod tests {
    use crate::parser::*;
    
    #[test]
    fn test_parser_pure(){
        let abc = "abc".to_string();

        assert_eq!(Parser::pure(1_i32).parse("abc"), Some((1, abc.clone())));
        assert_eq!(Parser::pure(1.0_f64).parse("abc"), Some((1.0, abc.clone())));
        assert_eq!(Parser::pure("1").parse("abc"), Some(("1", abc.clone())));
    }

    #[test]
    fn test_parser_functor(){
        let fi = move |i: i32| i.to_string();
        let fd = move |d: f64| d.to_string();
        let fs = |s: &str| s.parse().unwrap();
        let abc = "abc".to_string();

        assert_eq!(Parser::pure(1).map(fi).parse("abc"), Some(("1".to_string(), abc.clone())));
        assert_eq!(Parser::pure(1.0).map(fd).parse("abc"), Some(("1".to_string(), abc.clone())));
        assert_eq!(Parser::pure("1").map(fs).parse("abc"), Some((1, abc.clone())));

        assert_eq!(Parser::empty().map(fi).parse("abc"), None);
        assert_eq!(Parser::empty().map(fd).parse("abc"), None);
        assert_eq!(Parser::empty().map(fs).parse("abc"), None);
    }

    #[test]
    fn test_parser_applicative() {
        use std::rc::Rc;
        
        let psin = Parser::pure(Rc::new(|x: f64| x.sin()) as Rc<dyn Fn(f64) -> f64>);
        let pempty = Parser::empty() as Parser<'_, Rc<dyn Fn(f64) -> f64>>;
        let fd = Parser::pure(1.0);
        let nf = Parser::empty();

        assert_eq!(Parser::apply_p(psin.clone(), fd.clone()).parse("abc"), Some(((1.0 as f64).sin(), "abc".to_string())));
        assert_eq!(Parser::apply_p(psin.clone(), nf.clone()).parse("abc"), None);
        assert_eq!(Parser::apply_p(pempty.clone(), fd.clone()).parse("abc"), None);
        assert_eq!(Parser::apply_p(pempty.clone(), nf.clone()).parse("abc"), None);
    }

    #[test]
    fn test_parser_monad() {
        let i1 = Parser::pure(1);
        let pempty = Parser::empty();
        let eat = |x: i32| Parser::new(move |inp| Some((x.to_string() + inp, "".to_string())));
        let cancel = |_: i32| Parser::<'_, String>::new(move |_inp| None);

        assert_eq!(i1.clone().and_then(eat).parse("abc"), Some(("1abc".to_string(), "".to_string())));
        assert_eq!(i1.clone().and_then(cancel).parse("abc"), None);
        assert_eq!(pempty.clone().and_then(eat).parse("abc"), None);
        assert_eq!( pempty.clone().and_then(cancel).parse("abc"), None);
    }
}
