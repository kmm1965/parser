open Maybe

type 'a parser = P of (string -> ('a * string) maybe)

let parse (P unp) inp = unp inp

let (<$>) f p = P (fun inp -> (fun (x, out) -> (f x, out)) <$> (parse p inp))

let fmap f p = f <$> p

let pure x = P (fun inp -> just (x, inp))

let (>>=) p f = P (fun inp -> (parse p inp) >>= (fun (x, out) -> parse (f x) out))

let bind p f = p >>= f

let (<*>) fp p = fp >>= (fun f -> f <$> p ())

let apply fp p = fp <*> p

let (>>) p fq = p >>= (fun _ -> fq ())
let (>>>) p q = p >>= (fun _ -> q)

let skip p q = p >>> q

let empty = P (fun _ -> nothing)

let (<|>) p q = P (fun inp -> parse p inp <|> (fun () -> parse (q ()) inp))
let (<||>) p q = p <|> (fun () -> q)

let or_else p q = p <||> q
