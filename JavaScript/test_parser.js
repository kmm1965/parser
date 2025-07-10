let test_parser_pure = function (){
  verify("pure(1).parse(\"abc\")", Parser.pure(1).parse("abc"), Maybe.Just([1, "abc"]));
  verify("pure(\"1\").parse(\"abc\")", Parser.pure("1").parse("abc"), Maybe.Just(["1", "abc"]));
}

let test_parser_map = function (){
  let fi = (i) => i.toString();
  let fs = (s) => Number(s);
  
  verify("pure(1).map(fi).parse(\"abc\")", Parser.pure(1).map(fi).parse("abc"), Maybe.Just(["1", "abc"]));
  verify("pure(\"1\").map(fs).parse(\"abc\")", Parser.pure("1").map(fs).parse("abc"), Maybe.Just([1, "abc"]));

  verify("empty.map(fi).parse(\"abc\")", Parser.empty().map(fi).parse("abc"), Maybe.Nothing());
  verify("empty.map(fs).parse(\"abc\")", Parser.empty().map(fs).parse("abc"), Maybe.Nothing());
}

let test_parser_ap = function (){
  let psin = Parser.pure(Math.sin);
  let fd = () => Parser.pure(1);
  let nf = () => Parser.empty();

  verify("pure(sin).map(pure(1)).parse(\"abc\")", psin.ap(fd).parse("abc"), Maybe.Just([Math.sin(1), "abc"]));
  verify("pure(sin).map(empty).parse(\"abc\")", psin.ap(nf).parse("abc"), Maybe.Nothing());
  verify("empty.map(pure(1)).parse(\"abc\")", Parser.empty().ap(fd).parse("abc"), Maybe.Nothing());
  verify("empty.map(empty).parse(\"abc\")", Parser.empty().ap(nf).parse("abc"), Maybe.Nothing());
}

let test_parser_flatMap = function (){
  let i1 = Parser.pure(1);
  let iempty = Parser.empty();
  let eat = (x) => new Parser((inp) => Maybe.Just([x.toString() + inp, ""]));
  let cancel = (_) => new Parser((_) => Maybe.Nothing());
  
  verify("i1.flatMap(eat).parse(\"abc\")", i1.flatMap(eat).parse("abc"), Maybe.Just(["1abc", ""]));
  verify("i1.flatMap(cancel).parse(\"abc\")", i1.flatMap(cancel).parse("abc"), Maybe.Nothing());
  verify("iempty.flatMap(eat).parse(\"abc\")", iempty.flatMap(eat).parse("abc"), Maybe.Nothing());
  verify("iempty.flatMap(cancel).parse(\"abc\")", iempty.flatMap(cancel).parse("abc"), Maybe.Nothing());
}

let test_parser = function (){
  test_parser_pure();
  test_parser_map();
  test_parser_ap();
  test_parser_flatMap();
}
