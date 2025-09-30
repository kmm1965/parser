use "../src/some_parsers.sml";
use "test_core.sml";

open SomeParsers
open Maybe
open Testing

fun testAnyChar() =
  ptestg(anyChar "abc", Just((#"a", "bc")));
  ptestg(anyChar(""), Nothing);

fun testSatisfy() =
  ptestg(satisfy(fn (c) => c = #"a") "abc", Just((#"a", "bc")));
  ptestg(satisfy(fn (c) => c = #"z") "abc", Nothing);
  ptestg(satisfy(fn (c) => c = #"a") "", Nothing);

fun testChar() =
  ptestg(char(#"a") "abc", Just((#"a", "bc")));
  ptestg(char(#"z") "abc", Nothing);
  ptestg(char(#"a") "", Nothing);

fun testEmptyString() =
  ptestg(empty_string "abc", Just(("", "abc")));

fun testSpaces() =
  ptestg(spaces "abc", Just(("", "abc")));
  ptestg(spaces "   abc", Just(("   ", "abc")));

fun testSymbol() =
  ptestg(symbol(#"+") " + abc", Just((#"+", "abc")));
  ptestg(symbol(#"+") "abc", Nothing);

fun testAlnum() =
  ptestg(alnum "123abc", Just((#"1", "23abc")));
  ptestg(alnum "abc", Just((#"a", "bc")));
  ptestg(alnum "_123abc", Just((#"_", "123abc")));
  ptestg(alnum "!@#", Nothing);

fun testSign() =
  ptestg(sign "abc", Just(("", "abc")));
  ptestg(sign "+abc", Just(("+", "abc")));
  ptestg(sign "-abc", Just(("-", "abc")));

  ptestg(usign "abc", Just(("", "abc")));
  ptestg(usign " + abc", Just(("+", "abc")));
  ptestg(usign " - abc", Just(("-", "abc")));

fun testDigits() =
  ptestg(digits "123abc", Just(("123", "abc")));
  ptestg(digits "123  abc", Just(("123", "  abc")));
  ptestg(digits "abc", Just(("", "abc")));

fun testDouble() =
  ptestr(double("1 abc"), Just((1.0, "abc")));
  ptestr(double("1. abc"), Just((1.0, "abc")));
  ptestr(double("1.23 abc"), Just((1.23, "abc")));
  ptestr(double("-1.23 abc"), Nothing);
  ptestr(double(".23 abc"), Just((0.23, "abc")));
  ptestr(double(" + 1.23 abc"), Nothing);
  ptestr(double("1.23e10abc"), Just((1.23e10, "abc")));
  ptestr(double("1.23e-10abc"), Just((1.23e~10, "abc")));
  ptestr(double("abc"), Nothing);

fun testBetween() =
  	print "Testing Between...\n";

  val expr = between(symbol(#"("), symbol(#")"), fn () => double);

  ptestr(expr(" ( 123 ) abc"), Just((123.0, "abc")));
  ptestr(expr(" ( 123 abc"), Nothing);
  ptestr(expr(" 123 ) abc"), Nothing);

fun testChainlr1() =
  print "Testing Chain...\n";

  val add = skip(symbol(#"+"), fn () => pure(fn (x: real, y: real) => x + y));
  val sub = skip(symbol(#"-"), fn () => pure(fn (x: real, y: real) => x - y));
  val pow = skip(symbol(#"^"), fn () => pure(fn (x: real, y: real) => Math.exp(y * Math.ln(x))));

  val pexpr = chainl1(double, Parser.or_else(add, fn () => sub), false);

  ptestr(pexpr("7abc"), Just((7.0, "abc")));
  ptestr(pexpr(" 7 - 1 - 2 abc"), Just((4.0, "abc")));
  ptestr(pexpr(" 7 - 1 + 2 - 3 abc"), Just((5.0, "abc")));
  ptestr(pexpr("abc"), Nothing);

  ptestr(map(chainr1(double, pow)("3 ^ 2 ^ 3 abc"), fn (p) => (Real.fromInt(Real.floor(#1 p + 0.5)), #2 p)), Just((6561.0, "abc")));

testAnyChar();
testSatisfy();
testChar();
testEmptyString();
testSpaces();
testSymbol();
testAlnum();
testSign();
testDigits();
testDouble();
testBetween();
testChainlr1();
