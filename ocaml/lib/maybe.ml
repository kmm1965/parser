type 'a maybe = 'a option

let just x = Some x
let nothing = None

let (<|>) m1 fm2 = if Option.is_some m1 then m1 else fm2 ()
let (<||>) m1 m2 = m1 <|> (fun () -> m2)

let (<$>) = Option.map

let (>>=) = Option.bind
