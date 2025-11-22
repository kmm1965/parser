#[cfg(test)]
mod tests {
    use crate::calculator::Calculator;

    #[test]
    fn test_funcs() {
        let calc = Calculator::new();
        assert_eq!(calc.calc("sin(2.0)"), Some(((2.0 as f64).sin(), "".to_string())));
        assert_eq!(calc.calc("cos(2.0)"), Some(((2.0 as f64).cos(), "".to_string())));
        assert_eq!(calc.calc("asin(0.5)"), Some(((0.5 as f64).asin(), "".to_string())));
        assert_eq!(calc.calc("acos(0.5)"), Some(((0.5 as f64).acos(), "".to_string())));
        assert_eq!(calc.calc("sinh(2.0)"), Some(((2.0 as f64).sinh(), "".to_string())));
        assert_eq!(calc.calc("cosh(2.0)"), Some(((2.0 as f64).cosh(), "".to_string())));
        assert_eq!(calc.calc("tan(2.0)"), Some(((2.0 as f64).tan(), "".to_string())));
        assert_eq!(calc.calc("log(2.0)"), Some(((2.0 as f64).ln(), "".to_string())));
        assert_eq!(calc.calc("log10(2.0)"), Some(((2.0 as f64).log10(), "".to_string())));
        assert_eq!(calc.calc("exp(2.0)"), Some(((2.0 as f64).exp(), "".to_string())));
        assert_eq!(calc.calc("sqrt(2.0)"), Some(((2.0 as f64).sqrt(), "".to_string())));
        assert_eq!(calc.calc("sqr(2.0)"), Some((4.0, "".to_string())));
    }

    #[test]
    fn test_constants() {
        let calc = Calculator::new();
        assert_eq!(calc.calc("E"), Some((std::f64::consts::E, "".to_string())));
        assert_eq!(calc.calc("LOG2E"), Some((1.0 / (2.0 as f64).ln(), "".to_string())));
        assert_eq!(calc.calc("LOG10E"), Some((0.434294481903251827651, "".to_string())));
        assert_eq!(calc.calc("LN2"), Some(((2.0 as f64).ln(), "".to_string())));
        assert_eq!(calc.calc("LN10"), Some(((10.0 as f64).ln(), "".to_string())));
        assert_eq!(calc.calc("PI"), Some((std::f64::consts::PI, "".to_string())));
        assert_eq!(calc.calc("PI_2"), Some((std::f64::consts::PI / 2.0, "".to_string())));
        assert_eq!(calc.calc("PI_4"), Some((std::f64::consts::PI / 4.0, "".to_string())));
        assert_eq!(calc.calc("1_PI"), Some((1.0 / std::f64::consts::PI, "".to_string())));
        assert_eq!(calc.calc("2_PI"), Some((2.0 / std::f64::consts::PI, "".to_string())));
        assert_eq!(calc.calc("2_SQRTPI"), Some((2.0 / (std::f64::consts::PI).sqrt(), "".to_string())));
        assert_eq!(calc.calc("SQRT2"), Some(((2.0 as f64).sqrt(), "".to_string())));
        assert_eq!(calc.calc("SQRT1_2"), Some(((0.5 as f64).sqrt(), "".to_string())));
    }


    #[test]
    fn test_calculator() {
        let calc = Calculator::new();
        assert_eq!(calc.calc("7"), Some((7.0, "".to_string())));
        assert_eq!(calc.calc("7 - 1"), Some((6.0, "".to_string())));
        assert_eq!(calc.calc("72 - 7 - (1 - 2) * 3"), Some((72.0 - 7.0 - (1.0 - 2.0) * 3.0, "".to_string())));
        assert_eq!(calc.calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"), Some((7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3), "".to_string())));
        assert_eq!(calc.calc("3^(1+1)^3").map(|pair| (pair.0.round(), pair.1)), Some((6561.0, "".to_string())));
        assert_eq!(calc.calc("sin(1+1)"), Some(((2.0 as f64).sin(), "".to_string())));
        assert_eq!(calc.calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"), Some(((2.0 / std::f64::consts::PI.sqrt() * 4.0 - 1.0).sin(), "".to_string())));
        assert_eq!(calc.calc("sqr(2 + 3)"), Some((25.0, "".to_string())));
        assert_eq!(calc.calc("sin(-PI/4)"), Some(((-std::f64::consts::PI/4.0).sin(), "".to_string())));
        assert_eq!(calc.calc(" E ^ PI"), Some((std::f64::consts::E.powf(std::f64::consts::PI), "".to_string())));
        assert_eq!(calc.calc(" PI ^ E"), Some((std::f64::consts::PI.powf(std::f64::consts::E), "".to_string())));
    }
}
