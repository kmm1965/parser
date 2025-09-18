use "../src/Parser.sml";
use "test_core.sml";

open Maybe
open Parser
open Testing

fun test_parser_pure() =
  ptestg(pure(1) "abc", Just((1, "abc")));
  ptestr(pure(1.0) "abc", Just((1.0, "abc")));
  ptestg(pure("1") "abc", Just(("1", "abc")));

fun test_parser_map() =
  print "Testing map...\n";

  val fs = fn(s: string) => valOf(Int.fromString(s));

  ptestg(map(pure(1), Int.toString) "abc", Just(("1", "abc")));
  ptestg(map(pure(1.0), Real.toString) "abc", Just(("1.0", "abc")));
  ptestg(map(pure("1"), fs) "abc", Just((1, "abc")));

  ptestg(map(empty, Int.toString) "abc", Nothing);
  ptestg(map(empty, Real.toString) "abc", Nothing);
  ptestg(map(empty, fs) "abc", Nothing);

fun test_parser_apply() =
  print "Testing apply...\n";

  val psin = pure(Math.sin);
  val fd = fn () => pure(1.0);
  val nf = fn () => empty;

  ptestr(apply(psin, fd) "abc", Just((Math.sin(1.0), "abc")));
  ptestr(apply(psin, nf) "abc", Nothing);
  ptestr(apply(empty, fd) "abc", Nothing);
  ptestr(apply(empty, nf) "abc", Nothing);

fun test_parser_flat_map() =
  print "Testing flat_map...\n";

  val i1 = pure(1);
  val eat = fn (x: int) => fn (inp) => Just((Int.toString(x) ^ inp, ""));
  val cancel = fn(x: int) => empty;

  ptestg(flat_map(i1, eat) "abc", Just(("1abc", "")));
  ptestg(flat_map(i1, cancel) "abc", Nothing);
  ptestg(flat_map(empty, eat) "abc", Nothing);
  ptestg(flat_map(empty, cancel) "abc", Nothing);

test_parser_pure();
test_parser_map();
test_parser_apply();
test_parser_flat_map();
