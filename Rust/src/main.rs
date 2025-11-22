mod parser;
mod some_parsers;
mod calculator;

#[cfg(test)]
mod tests {
    mod option_tests;
    mod parser_tests;
    mod some_parsers_tests;
    mod calculator_tests;

    #[test]
    fn tests_main() {}
}

fn main() {
    use crate::calculator::Calculator;
    let calc = Calculator::new();
    println!("{0}", calc.calc("72 - 7 - (1 - 2) * 3").unwrap().0);                  // 68.0
    println!("{0}", calc.calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").unwrap().0); // -8.889
    println!("{0}", calc.calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").unwrap().0);      // -0.3634085731426532
    println!("{0}", calc.calc("sqr(sin(2)) + sqr(cos(1 + 1))").unwrap().0);         // 1.0
    println!("{0}", calc.calc("3 ^ 2 ^ 3").map(|pair| (pair.0.round(), pair.1)).unwrap().0); // 6561.0
    println!("{0}", calc.calc("sin(- PI/4)").unwrap().0);                           // -0.7071067811865476
    println!("{0}", calc.calc(" E ^ PI ").unwrap().0);                              // 23.140692632779263
    println!("{0}", calc.calc(" PI ^ E ").unwrap().0);                              // 22.45915771836104
}