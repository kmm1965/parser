structure Maybe =
struct
  datatype 'a Maybe = Nothing | Just of 'a

  fun map(m: 'a Maybe, f: 'a -> 'b): 'b Maybe =
    case m of
      Just x => Just (f x)
    | Nothing => Nothing;

  fun flat_map(m: 'a Maybe, f: 'a -> 'b Maybe): 'b Maybe =
    case m of
      Just x => f x
    | Nothing => Nothing;

  fun or_else(m: 'a Maybe, f: unit -> 'a Maybe): 'a Maybe =
    case m of
      Just x => m
    | Nothing => f();
end
