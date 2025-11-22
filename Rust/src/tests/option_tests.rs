#[cfg(test)]
mod tests {
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
