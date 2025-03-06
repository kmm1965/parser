module Maybe

// Maybe (Option) support
// Functor
let inline (</>) (f: 'A -> 'B) (x: Option<'A>) = if x.IsSome then Some(f x.Value) else None

// Applicative
let inline (<*>) (f: Option<'A -> 'B>) (x: Option<'A>) = if f.IsSome then f.Value </> x else None

// Monad
let inline (>>=) (x: Option<'A>) (f: 'A -> Option<'B>) = if x.IsSome then f x.Value else None

// Alternative
let inline (<|>) (x: Option<'A>) (fy: unit -> Option<'A>) = if x.IsSome then x else fy()
