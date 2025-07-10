let test_maybe_map = function (){
  verify("Just(1).map(sin)", Maybe.Just(1).map(Math.sin), Maybe.Just(Math.sin(1)));
  verify("Nothing.map(sin)", Maybe.Nothing().map(Math.sin), Maybe.Nothing());
}

let test_maybe_flatMap = function (){
  let safe_sqrt = (x) => x >= 0 ? Maybe.Just(Math.sqrt(x)) : Maybe.Nothing();
  let safe_log = (x) => x > 0 ? Maybe.Just(Math.log(x)) : Maybe.Nothing();

  verify("safe_sqrt(2)", safe_sqrt(2), Maybe.Just(Math.sqrt(2)));
  verify("safe_sqrt(0)", safe_sqrt(0), Maybe.Just(0));
  verify("safe_sqrt(-2)", safe_sqrt(-2), Maybe.Nothing());

  verify("safe_log(2)", safe_log(2), Maybe.Just(Math.log(2)));
  verify("safe_log(0)", safe_log(0), Maybe.Nothing());
  verify("safe_log(-2)", safe_log(-2), Maybe.Nothing());

  verify("Just(2).flatMap(safe_sqrt)", Maybe.Just(2).flatMap(safe_sqrt), Maybe.Just(Math.sqrt(2)));
  verify("Just(0).flatMap(safe_sqrt)", Maybe.Just(0).flatMap(safe_sqrt), Maybe.Just(0));
  verify("Nothing.flatMap(safe_sqrt)", Maybe.Nothing().flatMap(safe_sqrt), Maybe.Nothing());

  verify("Just(2).flatMap(safe_sqrt).flatMap(safe_log)", Maybe.Just(2).flatMap(safe_sqrt).flatMap(safe_log), Maybe.Just(Math.log(Math.sqrt(2))));
  verify("Just(-2).flatMap(safe_sqrt).flatMap(safe_log)", Maybe.Just(-2).flatMap(safe_sqrt).flatMap(safe_log), Maybe.Nothing());
  verify("Just(0).flatMap(safe_sqrt).flatMap(safe_log)", Maybe.Just(0).flatMap(safe_sqrt).flatMap(safe_log), Maybe.Nothing());

  verify("safe_sqrt(2).flatMap(safe_log)", safe_sqrt(2).flatMap(safe_log), Maybe.Just(Math.log(Math.sqrt(2))));
  verify("safe_sqrt(-2).flatMap(safe_log)", safe_sqrt(-2).flatMap(safe_log), Maybe.Nothing());
  verify("safe_sqrt(0).flatMap(safe_log)", safe_sqrt(0).flatMap(safe_log), Maybe.Nothing());
  verify("Nothing.flatMap(safe_log)", Maybe.Nothing().flatMap(safe_log), Maybe.Nothing());

  let to_string = (x) => x % 2 == 0 ? Maybe.Just(x.toString()) : Maybe.Nothing();

  verify("Just(2).flatMap(to_string)", Maybe.Just(2).flatMap(to_string), Maybe.Just("2"));
  verify("Just(1).flatMap(to_string)", Maybe.Just(1).flatMap(to_string), Maybe.Nothing());
  verify("Nothing.flatMap(to_string)", Maybe.Nothing().flatMap(to_string), Maybe.Nothing());
}

let test_maybe = function (){
  test_maybe_map();
  test_maybe_flatMap();
}
