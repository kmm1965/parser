module Maybe

// Maybe (Option) support

// Monad
let inline (>>=) (m: Option<'A>) (f: 'A -> Option<'B>) = if m.IsSome then f m.Value else None

// Functor
let inline (</>) (f: 'A -> 'B) (m: Option<'A>) = m >>= fun x -> Some(f x)

// Applicative
let inline (<*>) (mf: Option<'A -> 'B>) (m: Option<'A>) = mf >>= fun f -> f </> m

// Alternative
let inline (<|>) (m: Option<'A>) (fy: unit -> Option<'A>) = if m.IsSome then m else fy()
