local SomeParsers = require("../src/some_parsers")
local Parser = require("../src/parser")

function test(value, expected)
  print(expected == nil and value == expected or value ~= nil and value[1] == expected[1] and value[2] == expected[2])
end

function testAnyChar()
  test(SomeParsers.anyChar:parse("abc").value, {"a", "bc"})
  test(SomeParsers.anyChar:parse("").value, nil)
end

function testSatisfy()
  test(SomeParsers.satisfy(function (c) return c == 'a' end):parse("abc").value, {"a", "bc"})
  test(SomeParsers.satisfy(function (c) return c == 'z' end):parse("abc").value, nil)
  test(SomeParsers.satisfy(function (c) return c == 'a' end):parse("").value, nil)
end

function testChar()
  test(SomeParsers.char('a'):parse("abc").value, {"a", "bc"})
  test(SomeParsers.char('z'):parse("abc").value, nil)
  test(SomeParsers.char('a'):parse("").value, nil)
end

function testEmptyString()
  test(Parser.empty_string:parse("abc").value, {"", "abc"})
end

function testOptional()
  test(SomeParsers.optional_s(SomeParsers.char('1')):parse("1234").value, {"1", "234"})
  test(SomeParsers.optional_s(SomeParsers.char('1')):parse("abc").value, {"", "abc"})
end

function testSpaces()
  test(SomeParsers.spaces:parse("abc").value, {"", "abc"})
  test(SomeParsers.spaces:parse("   abc").value, {"   ", "abc"})
end

function testSymbol()
  test(SomeParsers.symbol('+'):parse(" + abc").value, {"+", "abc"})
  test(SomeParsers.symbol('+'):parse("abc").value, nil)
end

function testAlnum()
  test(SomeParsers.alnum:parse("123abc").value, {"1", "23abc"})
  test(SomeParsers.alnum:parse("abc").value, {"a", "bc"})
  test(SomeParsers.alnum:parse("_123abc").value, {"_", "123abc"})
  test(SomeParsers.alnum:parse("!@#").value, nil)
end

function testSign()
  test(SomeParsers.sign:parse("abc").value, {"", "abc"})
  test(SomeParsers.sign:parse("+abc").value, {"+", "abc"})
  test(SomeParsers.sign:parse("-abc").value, {"-", "abc"})

  test(SomeParsers.usign:parse("abc").value, {"", "abc"})
  test(SomeParsers.usign:parse(" + abc").value, {"+", "abc"})
  test(SomeParsers.usign:parse(" - abc").value, {"-", "abc"})
end

function testDigits()
  test(SomeParsers.digits:parse("123abc").value, {"123", "abc"})
  test(SomeParsers.digits:parse("123  abc").value, {"123", "  abc"})
  test(SomeParsers.digits:parse("abc").value, {"", "abc"})
end

function testDouble()
  test(SomeParsers.double:parse("1 abc").value, {1.0, "abc"})
  test(SomeParsers.double:parse("1. abc").value, {1.0, "abc"})
  test(SomeParsers.double:parse("1.23 abc").value, {1.23, "abc"})
  test(SomeParsers.double:parse("-1.23 abc").value, nil)
  test(SomeParsers.double:parse(".23 abc").value, {0.23, "abc"})
  test(SomeParsers.double:parse(" + 1.23 abc").value, nil)
  test(SomeParsers.double:parse("1.23e10abc").value, {1.23e10, "abc"})
  test(SomeParsers.double:parse("1.23e-10abc").value, {1.23e-10, "abc"})
  test(SomeParsers.double:parse("abc").value, nil)
end

function testBetween()
  local expr = SomeParsers.between(SomeParsers.symbol("("), SomeParsers.symbol(")"), function () return SomeParsers.double end)

  test(expr:parse(" ( 123 ) abc").value, {123.0, "abc"})
  test(expr:parse(" ( 123 abc").value, nil)
  test(expr:parse(" 123 ) abc").value, nil)
end

function testChainlr1()
  local add = SomeParsers.symbol('+'):skip(function () return Parser.pure(function (x, y) return x + y end) end)
  local sub = SomeParsers.symbol('-'):skip(function () return Parser.pure(function (x, y) return x - y end) end)
  local pow = SomeParsers.symbol('^'):skip(function () return Parser.pure(function (x, y) return math.exp(y * math.log(x)) end) end)

  local pexpr = SomeParsers.double:chainl1(add:or_else(function () return sub end), false)

  test(pexpr:parse("7abc").value, {7.0, "abc"})
  test(pexpr:parse(" 7 - 1 - 2 abc").value, {4.0, "abc"})
  test(pexpr:parse(" 7 - 1 + 2 - 3 abc").value, {5.0, "abc"})
  test(pexpr:parse("abc").value, nil)

  test(SomeParsers.double:chainr1(pow)
    :parse("3 ^ 2 ^ 3 abc")
    :map(function (p) return {math.floor(p[1] + 0.5), p[2]} end)
    .value, {6561.0, "abc"})
end

testAnyChar()
testSatisfy()
testChar()
testEmptyString()
testOptional()
testSpaces()
testSymbol()
testAlnum()
testSign()
testDigits()
testDouble()
testBetween()
testChainlr1()
