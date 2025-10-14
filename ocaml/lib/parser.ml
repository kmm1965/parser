open Maybe

type 'a parser = P of (string -> ('a * string) maybe)

let parse (P unp) inp = unp inp

let pure x = P (fun inp -> just (x, inp))

let (>>=) p f = P (fun inp -> (parse p inp) >>= (fun (x, out) -> parse (f x) out))

let bind p f = p >>= f

let (<$>) f p = p >>= fun a -> pure(f a)

let fmap f p = f <$> p

let (<*>) fp p = fp >>= (fun f -> f <$> p ())

let apply fp p = fp <*> p

let (>>) p fq = p >>= (fun _ -> fq ())
let (>>>) p q = p >>= (fun _ -> q)

let skip p q = p >>> q

let empty = P (fun _ -> nothing)

let (<|>) p q = P (fun inp -> parse p inp <|> (fun () -> parse (q ()) inp))
let (<||>) p q = p <|> (fun () -> q)

let or_else p q = p <||> q
