mod parser;
mod some_parsers;
mod calculator;

#[cfg(test)]
mod tests {
    mod parser_tests;
    mod some_parsers_tests;
    mod calculator_tests;

    fn safe_sqrt(x: f64) -> Option<f64>{
        if x >= 0.0 { Some(x.sqrt()) } else { None }
    }

    fn safe_log(x: f64) -> Option<f64>{
        if x > 0.0 { Some(x.ln()) } else { None }
    }

    fn to_string(i: i32) -> String { i.to_string() }

     fn to_string2(i: i32) -> Option<String> {
        if i % 2 == 0 { Some(i.to_string()) } else { None }
    }

    fn fsin(x: f64) -> f64 { x.sin() }
    
    #[test]
    fn test_maybe_map() {
        assert_eq!(safe_sqrt(2.0), Some((2.0_f64).sqrt()));
        assert_eq!(Some(1.0).map(fsin), Some(1.0_f64.sin()));
        assert_eq!(None.map(fsin), None);
        assert_eq!(Some(1).map(to_string), Some("1".to_string()));
        assert_eq!(None.map(to_string), None);
    }

    #[test]
    fn test_maybe_and_then() {
        assert_eq!(safe_sqrt(2.0), Some(2.0_f64.sqrt()));
        assert_eq!(safe_sqrt(0.0), Some(0.0_f64));
        assert_eq!(safe_sqrt(-2.0), None);

        assert_eq!(safe_log(2.0), Some(2.0_f64.ln()));
        assert_eq!(safe_log(0.0), None);
        assert_eq!(safe_log(-2.0), None);

        assert_eq!(Some(2.0).and_then(safe_sqrt), Some(2.0_f64.sqrt()));
        assert_eq!(Some(0.0).and_then(safe_sqrt), Some(0.0_f64));
        assert_eq!(Some(-2.0).and_then(safe_sqrt), None);

        assert_eq!(Some(2.0).and_then(safe_sqrt).and_then(safe_log), Some(2.0_f64.sqrt().ln()));
        assert_eq!(Some(0.0).and_then(safe_sqrt).and_then(safe_log), None);
        assert_eq!(Some(-2.0).and_then(safe_sqrt).and_then(safe_log), None);

        assert_eq!(Some(2).and_then(to_string2), Some("2".to_string()));
        assert_eq!(Some(1).and_then(to_string2), None);
        assert_eq!(None.and_then(to_string2), None);
    }

}

fn main() {
    use crate::calculator::Calculator;
    let calc = Calculator::new();
    println!("{0}", calc.clone().calc("72 - 7 - (1 - 2) * 3").unwrap().0);                  // 68.0
    println!("{0}", calc.clone().calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").unwrap().0); // -8.889
    println!("{0}", calc.clone().calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").unwrap().0);      // -0.3634085731426532
    println!("{0}", calc.clone().calc("sqr(sin(2)) + sqr(cos(1 + 1))").unwrap().0);         // 1.0
    println!("{0}", calc.clone().calc("3 ^ 2 ^ 3").map(|pair| (pair.0.round(), pair.1)).unwrap().0); // 6561.0
    println!("{0}", calc.clone().calc("sin(- PI/4)").unwrap().0);                           // -0.7071067811865476
    println!("{0}", calc.clone().calc(" E ^ PI ").unwrap().0);                              // 23.140692632779263
    println!("{0}", calc.clone().calc(" PI ^ E ").unwrap().0);                              // 22.45915771836104
}