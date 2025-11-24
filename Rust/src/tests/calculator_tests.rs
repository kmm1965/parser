#[cfg(test)]
mod tests {
    use crate::calculator::Calculator;

    #[test]
    fn test_funcs() {
        let expr = Calculator::new().expr();
        assert_eq!(expr.parse("sin(2.0)"), Some(((2.0 as f64).sin(), "".to_string())));
        assert_eq!(expr.parse("cos(2.0)"), Some(((2.0 as f64).cos(), "".to_string())));
        assert_eq!(expr.parse("asin(0.5)"), Some(((0.5 as f64).asin(), "".to_string())));
        assert_eq!(expr.parse("acos(0.5)"), Some(((0.5 as f64).acos(), "".to_string())));
        assert_eq!(expr.parse("sinh(2.0)"), Some(((2.0 as f64).sinh(), "".to_string())));
        assert_eq!(expr.parse("cosh(2.0)"), Some(((2.0 as f64).cosh(), "".to_string())));
        assert_eq!(expr.parse("tan(2.0)"), Some(((2.0 as f64).tan(), "".to_string())));
        assert_eq!(expr.parse("log(2.0)"), Some(((2.0 as f64).ln(), "".to_string())));
        assert_eq!(expr.parse("log10(2.0)"), Some(((2.0 as f64).log10(), "".to_string())));
        assert_eq!(expr.parse("exp(2.0)"), Some(((2.0 as f64).exp(), "".to_string())));
        assert_eq!(expr.parse("sqrt(2.0)"), Some(((2.0 as f64).sqrt(), "".to_string())));
        assert_eq!(expr.parse("sqr(2.0)"), Some((4.0, "".to_string())));
    }

    #[test]
    fn test_constants() {
        let expr = Calculator::new().expr();
        assert_eq!(expr.parse("E"), Some((std::f64::consts::E, "".to_string())));
        assert_eq!(expr.parse("LOG2E"), Some((1.0 / (2.0 as f64).ln(), "".to_string())));
        assert_eq!(expr.parse("LOG10E"), Some((0.434294481903251827651, "".to_string())));
        assert_eq!(expr.parse("LN2"), Some(((2.0 as f64).ln(), "".to_string())));
        assert_eq!(expr.parse("LN10"), Some(((10.0 as f64).ln(), "".to_string())));
        assert_eq!(expr.parse("PI"), Some((std::f64::consts::PI, "".to_string())));
        assert_eq!(expr.parse("PI_2"), Some((std::f64::consts::PI / 2.0, "".to_string())));
        assert_eq!(expr.parse("PI_4"), Some((std::f64::consts::PI / 4.0, "".to_string())));
        assert_eq!(expr.parse("1_PI"), Some((1.0 / std::f64::consts::PI, "".to_string())));
        assert_eq!(expr.parse("2_PI"), Some((2.0 / std::f64::consts::PI, "".to_string())));
        assert_eq!(expr.parse("2_SQRTPI"), Some((2.0 / (std::f64::consts::PI).sqrt(), "".to_string())));
        assert_eq!(expr.parse("SQRT2"), Some(((2.0 as f64).sqrt(), "".to_string())));
        assert_eq!(expr.parse("SQRT1_2"), Some(((0.5 as f64).sqrt(), "".to_string())));
    }


    #[test]
    fn test_calculator() {
        let expr = Calculator::new().expr();
        assert_eq!(expr.parse("7"), Some((7.0, "".to_string())));
        assert_eq!(expr.parse("7 - 1"), Some((6.0, "".to_string())));
        assert_eq!(expr.parse("72 - 7 - (1 - 2) * 3"), Some((72.0 - 7.0 - (1.0 - 2.0) * 3.0, "".to_string())));
        assert_eq!(expr.parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"), Some((7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3), "".to_string())));
        assert_eq!(expr.parse("3^(1+1)^3").map(|pair| (pair.0.round(), pair.1)), Some((6561.0, "".to_string())));
        assert_eq!(expr.parse("sin(1+1)"), Some(((2.0 as f64).sin(), "".to_string())));
        assert_eq!(expr.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"), Some(((2.0 / std::f64::consts::PI.sqrt() * 4.0 - 1.0).sin(), "".to_string())));
        assert_eq!(expr.parse("sqr(2 + 3)"), Some((25.0, "".to_string())));
        assert_eq!(expr.parse("sin(-PI/4)"), Some(((-std::f64::consts::PI/4.0).sin(), "".to_string())));
        assert_eq!(expr.parse(" E ^ PI"), Some((std::f64::consts::E.powf(std::f64::consts::PI), "".to_string())));
        assert_eq!(expr.parse(" PI ^ E"), Some((std::f64::consts::PI.powf(std::f64::consts::E), "".to_string())));
    }
}
