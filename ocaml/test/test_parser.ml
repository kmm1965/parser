open Lib_parser

open Maybe
(*open Parser*)
open Some_parsers
open Calculator

let maybe_int_pair_to_string = function
  | None -> "Nothing"
  | Some (i, s) -> "Just(" ^ string_of_int i ^ ",\"" ^ s ^ "\")"

let maybe_float_pair_to_string = function
  | None -> "Nothing"
  | Some (f, s) -> "Just(" ^ Float.to_string f ^ ",\"" ^ s ^ "\")"

let maybe_string_pair_to_string = function
  | None -> "Nothing"
  | Some (s, s2) -> "Just(\"" ^ s ^ "\",\"" ^ s2 ^ "\")"

let maybe_char_pair_to_string = function
  | None -> "Nothing"
  | Some (c, s) -> "Just('" ^ String.make 1 c ^ "',\"" ^ s ^ "\")"

let maybe_float_to_string = function
  | None -> "Nothing"
  | Some f -> "Just(" ^ Float.to_string f ^ ")"

let maybe_string_to_string = function
  | None -> "Nothing"
  | Some s -> "Just(\"" ^ s ^ "\")"

let safe_sqrt x = if x >= 0. then just (sqrt x) else nothing
let safe_log  x = if x >  0. then just (log x)  else nothing

let test_strings msg s1 s2 =
    Printf.printf "%s=%s=%s (%s)\n" msg s1 s2 (if String.equal s1 s2 then "equals" else "NOT equals")

let test_float msg m1 m2 =
    test_strings msg (maybe_float_to_string m1) (maybe_float_to_string m2)

let test_string msg m1 m2 =
    test_strings msg (maybe_string_to_string m1) (maybe_string_to_string m2)

let test_int_pair msg m1 m2 =
    test_strings msg (maybe_int_pair_to_string m1) (maybe_int_pair_to_string m2)

let test_float_pair msg m1 m2 =
    test_strings msg (maybe_float_pair_to_string m1) (maybe_float_pair_to_string m2)

let test_string_pair msg m1 m2 =
    test_strings msg (maybe_string_pair_to_string m1) (maybe_string_pair_to_string m2)

let test_char_pair msg m1 m2 =
    test_strings msg (maybe_char_pair_to_string m1) (maybe_char_pair_to_string m2)

let test_maybe_map () =
    test_float "sin <$> Just 1." (sin <$> just 1.) (just (sin 1.));
    test_float "sin <$> Nothing" (sin <$> nothing) nothing;
    test_string "string_of_int <$> Just 1" (string_of_int <$> just 1) (just "1");
    test_string "string_of_int <$> Nothing" (string_of_int <$> nothing) nothing

let to_string x = if x mod 2 == 0 then just (string_of_int x) else nothing

let test_maybe_flat_map () =
    test_float "safe_sqrt 2." (safe_sqrt 2.) (just (sqrt 2.));
    test_float "safe_sqrt 0." (safe_sqrt 0.) (just 0.);
    test_float "safe_sqrt -2." (safe_sqrt (-2.)) nothing;
    test_float "safe_log 2." (safe_log 2.) (just (log 2.));
    test_float "safe_log 0." (safe_log 0.) nothing;
    test_float "safe_log -2." (safe_log (-2.)) nothing;
    test_float "Just 2. >>= safe_sqrt" (just 2. >>= safe_sqrt) (just (sqrt 2.));
    test_float "Just 0. >>= safe_sqrt" (just 0. >>= safe_sqrt) (just 0.);
    test_float "Nohing >>= safe_sqrt" (nothing >>= safe_sqrt) nothing;
    test_float "Just 2. >>= safe_sqrt >>= safe_log" (just 2. >>= safe_sqrt >>= safe_log) (just (log (sqrt 2.)));
    test_float "Just -2. >>= safe_sqrt >>= safe_log" (just (-2.) >>= safe_sqrt >>= safe_log) nothing;
    test_float "Just 0. >>= safe_sqrt >>= safe_log" (just 0. >>= safe_sqrt >>= safe_log) nothing;
    test_float "(safe_sqrt 2.) >>= safe_log" ((safe_sqrt 2.) >>= safe_log) (just (log (sqrt 2.)));
    test_float "(safe_sqrt -2.) >>= safe_log" ((safe_sqrt (-2.)) >>= safe_log) nothing;
    test_float "(safe_sqrt 0.) >>= safe_log" ((safe_sqrt 0.) >>= safe_log) nothing;
    test_float "Nothing >>= safe_log" (nothing >>= safe_log) nothing;
    test_string "Just 2 >>= to_string" (just 2 >>= to_string) (just "2");
    test_string "Just 1 >>= to_string" (just 1 >>= to_string) nothing;
    test_string "Nothing >>= to_string" (nothing >>= to_string) nothing

let test_maybe () =
    Printf.printf "%s=Nothing\n" (maybe_float_pair_to_string nothing);
    Printf.printf "%s=Just(1.,\"abc\")\n" (maybe_float_pair_to_string (just (1.,"abc")));
    Printf.printf "%s=Nothing\n" (maybe_float_to_string nothing);
    Printf.printf "%s=Just(1.)\n" (maybe_float_to_string (just 1.));
    test_maybe_map ();
    test_maybe_flat_map ()

let test_parser_pure () =
    test_int_pair "parse (pure 1) \"abc\"" ((Parser.pure 1 |> Parser.parse) "abc") (just (1, "abc"));
    test_float_pair "parse (pure 1.) \"abc\"" ((Parser.pure 1. |> Parser.parse) "abc") (just (1., "abc"));
    test_string_pair "parse (pure \"1\") \"abc\"" ((Parser.pure "1" |> Parser.parse) "abc") (just ("1", "abc"))

let test_parser_functor () =
    let fi = fun i -> string_of_int i in
    let fd = fun d -> Float.to_string d in
    let fs = fun s -> int_of_string s in
    test_string_pair "parse (fi <$> pure 1) \"abc\"" ((Parser.pure 1 |> Parser.fmap fi |> Parser.parse) "abc") (just ("1", "abc"));
    test_string_pair "parse (fd <$> pure 1.) \"abc\"" ((Parser.pure 1. |> Parser.fmap fd |> Parser.parse) "abc") (just ("1.", "abc"));
    test_int_pair "parse (fs <$> pure \"1\") \"abc\"" ((Parser.pure "1" |> Parser.fmap fs |> Parser.parse) "abc") (just (1, "abc"));
    test_string_pair "parse (fi <$> empty) \"abc\"" ((Parser.empty |> Parser.fmap fi |> Parser.parse) "abc") nothing;
    test_string_pair "parse (fd <$> empty) \"abc\"" ((Parser.empty |> Parser.fmap fd |> Parser.parse) "abc") nothing;
    test_int_pair "parse (fs <$> empty) \"abc\"" ((Parser.empty |> Parser.fmap fs |> Parser.parse) "abc") nothing

let test_parser_applicative () =
    let psin = Parser.pure sin in
    let fd = fun () -> Parser.pure 1. in
    let nf = fun () -> Parser.empty in
    test_float_pair "parse (pure sin <*> pure 1.) \"abc\"" ((Parser.apply psin fd |> Parser.parse) "abc") (just (sin 1., "abc"));
    test_float_pair "parse (pure sin <*> empty) \"abc\"" ((Parser.apply psin nf |> Parser.parse) "abc") nothing;
    test_float_pair "parse (empty <*> pure 1.) \"abc\"" ((Parser.apply Parser.empty fd |> Parser.parse) "abc") nothing;
    test_float_pair "parse (empty <*> empty) \"abc\"" ((Parser.apply Parser.empty nf |> Parser.parse) "abc") nothing

let test_parser_monad () =
    let i1 = Parser.pure 1 in
    let eat = fun x -> Parser.P (fun inp -> just(string_of_int x ^ inp, "")) in
    let cancel = fun _ -> Parser.P (fun _ -> nothing) in
    test_string_pair "parse (pure 1 >>= eat) \"abc\"" ((Parser.bind i1 eat |> Parser.parse) "abc") (just ("1abc", ""));
    test_string_pair "parse (pure 1 >>= cancel) \"abc\"" ((Parser.bind i1 cancel |> Parser.parse) "abc") nothing;
    test_string_pair "parse (empty >>= eat) \"abc\"" ((Parser.bind Parser.empty eat |> Parser.parse) "abc") nothing;
    test_string_pair "parse (empty >>= cancel) \"abc\"" ((Parser.bind Parser.empty cancel |> Parser.parse) "abc") nothing

let test_parser () =
    test_parser_pure ();
    test_parser_functor ();
    test_parser_applicative ();
    test_parser_monad ()

let test_anyChar () =
    test_char_pair "parse anyChar \"abc\"" (Parser.parse anyChar "abc") (just ('a', "bc"));
    test_char_pair "parse anyChar \"\"" (Parser.parse anyChar "") nothing

let test_satisfy () =
    test_char_pair "parse (satisfy (fun c -> c == 'a')) \"abc\"" ((satisfy (fun c -> c == 'a') |> Parser.parse) "abc") (just ('a', "bc"));
    test_char_pair "parse (satisfy (fun c -> c == 'z')) \"abc\"" ((satisfy (fun c -> c == 'z') |> Parser.parse) "abc") nothing

let test_char () =
    test_char_pair "parse (char 'a') \"abc\"" ((char 'a' |> Parser.parse) "abc") (just ('a', "bc"));
    test_char_pair "parse (char 'z') \"abc\"" ((char 'z' |> Parser.parse) "abc") nothing

let test_empty_string () =
    test_string_pair "parse empty_string \"abc\"" (Parser.parse empty_string "abc") (just ("", "abc"))

let test_optional () =
    test_string_pair "parse (optional_c (char '1')) \"1234\"" ((char '1' |> optional_c |> Parser.parse) "1234") (just ("1", "234"));
    test_string_pair "parse (optional_c (char '1')) \"abc\""  ((char '1' |> optional_c |> Parser.parse) "abc")  (just ("", "abc"))

let test_spaces () =
    test_string_pair "parse spaces \"abc\"" (Parser.parse spaces "abc") (just ("", "abc"));
    test_string_pair "parse spaces \"  abc\"" (Parser.parse spaces "  abc") (just ("  ", "abc"))

let test_symbol () =
    test_char_pair "parse (symbol '+') \" + abc\"" ((symbol '+' |> Parser.parse) " + abc") (just ('+', "abc"));
    test_char_pair "parse (symbol '+') \"abc\"" ((symbol '+' |> Parser.parse) "abc") nothing

let test_alnum () =
    test_char_pair "parse alnum \"123abc  \"" (Parser.parse alnum "123abc  ") (just ('1', "23abc  "));
    test_char_pair "parse alnum \"_123abc  \"" (Parser.parse alnum "_123abc  ") (just ('_', "123abc  "));
    test_char_pair "parse alnum \"!@#$\"" (Parser.parse alnum "!@#$") nothing

let test_name () =
    let psin = name "sin" in
    test_string_pair "parse (name \"sin\") \" sin \"" (Parser.parse psin " sin ") (just ("sin", ""));
    test_string_pair "parse (name \"sin\") \" sin (1.)\"" (Parser.parse psin " sin (1.)") (just ("sin", "(1.)"));
    test_string_pair "parse (name \"sin\") \"sinabc\"" (Parser.parse psin "sinabc") nothing

let test_sign () =
    test_string_pair "parse sign \"abc\"" (Parser.parse sign "abc") (just ("", "abc"));
    test_string_pair "parse sign \"+abc\"" (Parser.parse sign "+abc") (just ("+", "abc"));
    test_string_pair "parse sign \"-abc\"" (Parser.parse sign "-abc") (just ("-", "abc"));
    test_string_pair "parse usign \"abc\"" (Parser.parse usign "abc") (just ("", "abc"));
    test_string_pair "parse usign \" + abc\"" (Parser.parse usign " + abc") (just ("+", "abc"));
    test_string_pair "parse usign \" - abc\"" (Parser.parse usign " - abc") (just ("-", "abc"))

let test_digits () =
    test_string_pair "parse digits \"123abc\"" (Parser.parse digits "123abc") (just ("123", "abc"));
    test_string_pair "parse digits \"123  abc\"" (Parser.parse digits "123  abc") (just ("123", "  abc"));
    test_string_pair "parse digits \"abc\"" (Parser.parse digits "abc") (just ("", "abc"))

let test_double () =
    test_float_pair "parse double \"1 abc\"" (Parser.parse double "1 abc") (just (1., "abc"));
    test_float_pair "parse double \"1. abc\"" (Parser.parse double "1. abc") (just (1., "abc"));
    test_float_pair "parse double \"1.23 abc\"" (Parser.parse double "1.23 abc") (just (1.23, "abc"));
    test_float_pair "parse double \".23 abc\"" (Parser.parse double ".23 abc") (just (0.23, "abc"));
    test_float_pair "parse double \" + 1.23 abc\"" (Parser.parse double " + 1.23 abc") nothing;
    test_float_pair "parse double \"1.23e10abc\"" (Parser.parse double "1.23e10abc") (just (1.23e10, "abc"));
    test_float_pair "parse double \"1.23e-10abc\"" (Parser.parse double "1.23e-10abc") (just (1.23e-10, "abc"));
    test_float_pair "parse double \"abc\"" (Parser.parse double "abc") nothing

let test_between () =
    let expr = between (symbol '(') (symbol ')') double in
    test_float_pair "parse (between (symbol '(') (symbol ')') double) \" ( 123 ) abc\"" (Parser.parse expr " ( 123 ) abc") (just (123., "abc"));
    test_float_pair "parse (between (symbol '(') (symbol ')') double) \" ( 123 abc\"" (Parser.parse expr " ( 123 abc") nothing;
    test_float_pair "parse (between (symbol '(') (symbol ')') double) \" 123 ) abc\"" (Parser.parse expr " 123 ( abc") nothing

let test_chainlr1 () =
    let add = Parser.skip (symbol '+') (Parser.pure (fun x y -> x +. y)) in
    let sub = Parser.skip (symbol '-') (Parser.pure (fun x y -> x -. y)) in
    let pow = Parser.skip (symbol '^') (Parser.pure (fun x y -> exp (y *. (log x)))) in
    let pexpr = chainl1 double (Parser.or_else add sub) false in
    test_float_pair "parse pexpr \"7abc\"" (Parser.parse pexpr "7abc") (just (7., "abc"));
    test_float_pair "parse pexpr \" 7 - 1 - 2 abc\"" (Parser.parse pexpr " 7 - 1 - 2 abc") (just (4., "abc"));
    test_float_pair "parse pexpr \" 7 - 1 + 2 - 3 abc\"" (Parser.parse pexpr " 7 - 1 + 2 - 3 abc") (just (5., "abc"));
    test_float_pair "parse pexpr \"abc\"" (Parser.parse pexpr "abc") nothing;
    test_float_pair "parse (chainr1 double pow) \"3 ^ 2 ^ 3 abc\"" (Parser.parse (chainr1 double pow) "3 ^ 2 ^ 3 abc") (just (6561., "abc"))

let test_some_parsers () =
    test_anyChar ();
    test_satisfy ();
    test_char ();
    test_empty_string ();
    test_optional ();
    test_spaces ();
    test_symbol ();
    test_alnum ();
    test_name ();
    test_sign ();
    test_digits ();
    test_double ();
    test_between ();
    test_chainlr1 ()

let test_funcs () =
    test_float_pair "calculate \"sin(2.0)\"" (calculate "sin(2.0)") (just (sin(2.0), ""));
    test_float_pair "calculate \"cos(2.0)\"" (calculate "cos(2.0)") (just (cos(2.0), ""));
    test_float_pair "calculate \"asin(0.5)\"" (calculate "asin(0.5)") (just (asin(0.5), ""));
    test_float_pair "calculate \"acos(0.5)\"" (calculate "acos(0.5)") (just (acos(0.5), ""));
    test_float_pair "calculate \"sinh(2.0)\"" (calculate "sinh(2.0)") (just (sinh(2.0), ""));
    test_float_pair "calculate \"cosh(2.0)\"" (calculate "cosh(2.0)") (just (cosh(2.0), ""));
    test_float_pair "calculate \"asinh(2.0)\"" (calculate "asinh(2.0)") (just (asinh(2.0), ""));
    test_float_pair "calculate \"acosh(2.0)\"" (calculate "acosh(2.0)") (just (acosh(2.0), ""));
    test_float_pair "calculate \"tan(2.0)\"" (calculate "tan(2.0)") (just (tan(2.0), ""));
    test_float_pair "calculate \"log(2.0)\"" (calculate "log(2.0)") (just (log(2.0), ""));
    test_float_pair "calculate \"log10(2.0)\"" (calculate "log10(2.0)") (just (log10(2.0), ""));
    test_float_pair "calculate \"exp(2.0)\"" (calculate "exp(2.0)") (just (exp(2.0), ""));
    test_float_pair "calculate \"sqrt(2.0)\"" (calculate "sqrt(2.0)") (just (sqrt(2.0), ""));
    test_float_pair "calculate \"sqr(2.0)\"" (calculate "sqr(2.0)") (just (4.0, ""))

let test_consts () =
    let pi = 3.14159265358979323846 in
    test_float_pair "calculate \"E\"" (calculate "E") (just (2.71828182845904523536, ""));
    test_float_pair "calculate \"LOG2E\"" (calculate "LOG2E") (just (1.0 /. log(2.0), ""));
    test_float_pair "calculate \"LOG10E\"" (calculate "LOG10E") (just (0.4342944819032518, ""));
    test_float_pair "calculate \"LOG10E\"" (calculate "LOG10E") (just (1.0 /. log(10.0), ""));
    test_float_pair "calculate \"LN2\"" (calculate "LN2") (just (log(2.0), ""));
    test_float_pair "calculate \"LN10\"" (calculate "LN10") (just (log(10.0), ""));
    test_float_pair "calculate \"PI\"" (calculate "PI") (just (pi, ""));
    test_float_pair "calculate \"PI_2\"" (calculate "PI_2") (just (pi /. 2.0, ""));
    test_float_pair "calculate \"PI_4\"" (calculate "PI_4") (just (pi /. 4.0, ""));
    test_float_pair "calculate \"1_PI\"" (calculate "1_PI") (just (1.0 /. pi, ""));
    test_float_pair "calculate \"2_PI\"" (calculate "2_PI") (just (2.0 /. pi, ""));
    test_float_pair "calculate \"2_SQRTPI\"" (calculate "2_SQRTPI") (just (2.0 /. sqrt(pi), ""));
    test_float_pair "calculate \"SQRT2\"" (calculate "SQRT2") (just (sqrt(2.0), ""));
    test_float_pair "calculate \"SQRT1_2\"" (calculate "SQRT1_2") (just (sqrt(0.5), ""))

let test_calculator () =
    test_float_pair "calculate \"72 - 7 - (1 - 2) * 3\"" (calculate "72 - 7 - (1 - 2) * 3") (just (68., ""));
    test_float_pair "calculate \" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)\"" (calculate " 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") (just (-8.889, ""));
    test_float_pair "calculate \"3^(1+1)^3\"" (calculate "3^(1+1)^3") (just (6561., ""));
    test_float_pair "calculate \"sin(1+1)\"" (calculate "sin(1+1)") (just (sin(2.), ""));
    test_float_pair "calculate \"sin ( 2_SQRTPI * sqr ( 2 ) - 1 )\"" (calculate "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )") (just (-0.363408573143, ""));
    test_float_pair "calculate \"sqr(2 + 3)\"" (calculate "sqr(2 + 3)") (just (25., ""));
    test_float_pair "calculate \"sin(-PI/4)\"" (calculate "sin(-PI/4)") (just (-0.707106781187, ""));
    test_float_pair "calculate \" E ^ PI\"" (calculate " E ^ PI") (just (23.1406926328, ""));
    test_float_pair "calculate \" PI ^ E\"" (calculate " PI ^ E") (just (22.4591577184, ""))

let () =
    test_maybe ();
    test_parser ();
    test_some_parsers ();
    test_funcs();
    test_consts();
    test_calculator ()
