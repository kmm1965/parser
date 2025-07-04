open Maybe
open Parser

let anyChar = P (fun inp -> if String.length inp > 0
    then just (inp.[0], String.sub inp 1 ((String.length inp) - 1))
    else nothing)

let satisfy pred = anyChar >>= (fun x -> if pred x then pure x else empty)

let char c = satisfy (fun x -> x == c)

let empty_string = pure String.empty

let rec some p = (fun c -> fun s -> String.cat (String.make 1 c) s) <$> p <*> (fun () -> many p)
and many p = (some p) <||> empty_string

let spaces = many (char ' ')

let between_get open_ close_ fp = (open_ >> fp) >>= (fun x -> close_ >> (fun () -> pure x))

let between open_ close_ p = between_get open_ close_ (fun () -> p)

let token p = between spaces spaces p

let symbol c = token (char c)

let is_alpha alpha = 
    match alpha with 
      'a' .. 'z' -> true
    | 'A' .. 'Z' -> true 
    | _ -> false;;

let is_digit digit =
    match digit with
     '0' .. '9' -> true
    | _ -> false;; 

let alnum = satisfy (fun c -> is_alpha c || is_digit c || c == '_')

let name n = token (some alnum) >>= (fun s -> if String.equal s n then pure n else empty)

let optional_s p = p <||> empty_string

let optional_c p = optional_s ((fun c -> String.make 1 c) <$> p)

let sign = optional_c (char '+' <||> char '-')

let usign = optional_c (symbol '+' <||> symbol '-')

let digits = many (satisfy is_digit)

let double = token(sign >>=
    (fun sign_part -> digits >>=
    (fun int_part  -> optional_s (char '.' >>> digits) >>=
    (fun frac_part -> optional_s (((char 'e' <||> char 'E') >>> sign) >>=
        (fun exp_sign -> some (satisfy is_digit) >>=
        (fun exp_digits -> pure (String.cat exp_sign exp_digits)))) >>=
    (fun exp_part  -> if String.length int_part > 0 || String.length frac_part > 0 then
        pure (Float.of_string (sign_part ^ int_part ^
            (if String.length frac_part > 0 then "." ^ frac_part else "") ^
            (if String.length exp_part > 0 then "e" ^ exp_part else "")))
        else empty)))))

let rest p ff op x = (op >>= (fun f -> p >>= (fun y -> ff (f x y)))) <|> (fun () -> pure x)

let rec rest_l p op x = rest p (fun y -> rest_l p op y) op x

let chainl1 p op negate_first = p >>= (fun x -> rest_l p op (if negate_first then -.x else x))

let rec rest_r p op x = rest (chainr1 p op) pure op x
and chainr1 p op = p >>= (fun x -> rest_r p op x)
