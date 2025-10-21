let test_anyChar = function (){
  verify("anyChar.parse(\"abc\")", anyChar.parse("abc"), Maybe.Just(['a', "bc"]));
  verify("anyChar.parse(\"\")", anyChar.parse(""), Maybe.Nothing());
}

let test_satisfy = function(){
  verify("satisfy(c=='a').parse(\"abc\")", satisfy(c => c === 'a').parse("abc"), Maybe.Just(['a', "bc"]));
  verify("satisfy(c=='z').parse(\"abc\")", satisfy(c => c === 'z').parse("abc"), Maybe.Nothing());
}

let test_char = function(){
  verify("char('a').parse(\"abc\")", char_('a').parse("abc"), Maybe.Just(['a', "bc"]));
  verify("char('z').parse(\"abc\")", char_('z').parse("abc"), Maybe.Nothing());
}

let test_empty_string = function(){
  verify("empty_string.parse(\"abc\")", empty_string.parse("abc"), Maybe.Just(["", "abc"]));
}

let test_optional = function(){
  verify("optional_s(char('1')).parse(\"1234\")", optional_s(char_('1')).parse("1234"), Maybe.Just(["1", "234"]));
  verify("optional_s(char('1')).parse(\"abc\")", optional_s(char_('1')).parse("abc"), Maybe.Just(["", "abc"]));
}

let test_spaces = function(){
  verify("spaces.parse(\"abc\")", spaces.parse("abc"), Maybe.Just(["", "abc"]));
  verify("spaces.parse(\"  abc\")", spaces.parse("  abc"), Maybe.Just(["  ", "abc"]));
}

let test_symbol = function(){
  verify("symbol('+').parse(\" + abc\")", symbol('+').parse(" + abc"), Maybe.Just(['+', "abc"]));
  verify("symbol('+').parse(\"abc\")", symbol('+').parse("abc"), Maybe.Nothing());
}

let test_alnum = function(){
  verify("alnum.parse(\"123abc\")", alnum.parse("123abc"), Maybe.Just(['1', "23abc"]));
  verify("alnum.parse(\"_123abc\")", alnum.parse("_123abc"), Maybe.Just(['_', "123abc"]));
  verify("alnum.parse(\"!@#$\")", alnum.parse("!@#$"), Maybe.Nothing());

  verify("alnum.some().parse(\"123abc\")", alnum.some().parse("123abc"), Maybe.Just(["123abc", ""]));
}

let test_name = function(){
  let psin = name("sin");
  
  verify("psin.parse(\" sin \")", psin.parse(" sin "), Maybe.Just(["sin", ""]));
  verify("psin.parse(\" sin (1.)\")", psin.parse(" sin (1.)"), Maybe.Just(["sin", "(1.)"]));
  verify("psin.parse(\"sinabc\")", psin.parse("sinabc"), Maybe.Nothing());
}

let test_sign = function(){
  verify("sign.parse(\"abc\")", sign.parse("abc"), Maybe.Just(["", "abc"]));
  verify("sign.parse(\"+abc\")", sign.parse("+abc"), Maybe.Just(["+", "abc"]));
  verify("sign.parse(\"-abc\")", sign.parse("-abc"), Maybe.Just(["-", "abc"]));

  verify("usign.parse(\"abc\")", usign.parse("abc"), Maybe.Just(["", "abc"]));
  verify("usign.parse(\"+abc\")", usign.parse(" + abc"), Maybe.Just(["+", "abc"]));
  verify("usign.parse(\"-abc\")", usign.parse(" - abc"), Maybe.Just(["-", "abc"]));
}

let test_digits = function(){
  verify("digits.parse(\"123abc\")", digits.parse("123abc"), Maybe.Just(["123", "abc"]));
  verify("digits.parse(\"123  abc\")", digits.parse("123  abc"), Maybe.Just(["123", "  abc"]));
  verify("digits.parse(\"abc\")", digits.parse("abc"), Maybe.Just(["", "abc"]));
}

let test_double = function(){
  verify("double.parse(\"1 abc\")", double_.parse("1 abc"), Maybe.Just([1, "abc"]));
  verify("double.parse(\"1. abc\")", double_.parse("1. abc"), Maybe.Just([1, "abc"]));
  verify("double.parse(\"1.23 abc\")", double_.parse("1.23 abc"), Maybe.Just([1.23, "abc"]));
  verify("double.parse(\".23 abc\")", double_.parse(".23 abc"), Maybe.Just([0.23, "abc"]));
  verify("double.parse(\" + 1.23 abc\")", double_.parse(" + 1.23 abc"), Maybe.Nothing());
  verify("double.parse(\"1.23e10abc\")", double_.parse("1.23e10abc"), Maybe.Just([1.23e10, "abc"]));
  verify("double.parse(\"1.23e-10abc\")", double_.parse("1.23e-10abc"), Maybe.Just([1.23e-10, "abc"]));
  verify("double.parse(\"abc\")", double_.parse("abc"), Maybe.Nothing());
}

let test_between = function(){
  let expr = between(symbol('('), symbol(')'), () => double_);
  
  verify("expr.parse(\" ( 123 ) abc\")", expr.parse("( 123 ) abc"), Maybe.Just([123, "abc"]));
  verify("expr.parse(\" ( 123 abc\")", expr.parse("( 123 abc"), Maybe.Nothing());
  verify("expr.parse(\" 123 ) abc\")", expr.parse(" 123 ) abc"), Maybe.Nothing());
}

let test_chainlr1 = function(){
  let add = symbol('+').skip(() => Parser.pure((x, y) => x + y));
  let sub = symbol('-').skip(() => Parser.pure((x, y) => x - y));
  let pexpr = double_.chainl1(add.orElse(sub), false);
  let pow = symbol('^').skip(() => Parser.pure((x, y) => Math.exp(y * Math.log(x))));

  verify("pexpr.parse(\"7abc\")", pexpr.parse("7abc"), Maybe.Just([7, "abc"]));
  verify("pexpr.parse(\"7 - 1 - 2 abc\")", pexpr.parse("7 - 1 - 2 abc"), Maybe.Just([4, "abc"]));
  verify("pexpr.parse(\"7 - 1 + 2 - 3 abc\")", pexpr.parse("7 - 1 + 2 - 3 abc"), Maybe.Just([5, "abc"]));
  verify("pexpr.parse(\"abc\")", pexpr.parse("abc"), Maybe.Nothing());

  verify("double_.chainr1(pow).parse(\"3 ^ 2 ^ 3 abc\")",
    double_.chainr1(pow).parse("3 ^ 2 ^ 3 abc").map(([x, s]) => [Math.round(x), s]),
    Maybe.Just([6561, "abc"]));
}

let test_some_parsers = function (){
  test_anyChar();
  test_satisfy();
  test_char();
  test_empty_string();
  test_optional();
  test_spaces();
  test_symbol();
  test_alnum();
  test_name();
  test_sign();
  test_digits();
  test_double();
  test_between();
  test_chainlr1();
}
