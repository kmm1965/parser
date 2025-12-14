local Parser = require("../src/parser")
local SomeParsers = require("../src/some_parsers")

local Calculator = {}

function op2(c, f)
  return SomeParsers.symbol(c):skip(function () return Parser.pure(f) end)
end

Calculator.add = op2('+', function (x, y) return x + y end)
Calculator.sub = op2('-', function (x, y) return x - y end)
Calculator.mul = op2('*', function (x, y) return x * y end)
Calculator.div = op2('/', function (x, y) return x / y end)
Calculator.pow = op2('^', function (x, y) return math.exp(y * math.log(x)) end)

function fold(parsers)
  local p0 = Parser.empty
  for index, p in ipairs(parsers) do
    p0 = p0:or_else(function () return p end)
  end
  return SomeParsers.token(p0)
end

function guard(b, value)
  return b and Parser.pure(value) or Parser.empty
end

Calculator.funcs = SomeParsers.identifier:flat_map(function (n) return fold({
  guard(n == "sin",   function (x) return math.sin(x) end),
  guard(n == "cos",   function (x) return math.cos(x) end),
  guard(n == "asin",  function (x) return math.asin(x) end),
  guard(n == "acos",  function (x) return math.acos(x) end),
  guard(n == "sinh",  function (x) return math.sinh(x) end),
  guard(n == "cosh",  function (x) return math.cosh(x) end),
  guard(n == "tan",   function (x) return math.tan(x) end),
  guard(n == "log",   function (x) return math.log(x) end),
  guard(n == "log10", function (x) return math.log10(x) end),
  guard(n == "exp",   function (x) return math.exp(x) end),
  guard(n == "sqrt",  function (x) return math.sqrt(x) end),
  guard(n == "sqr",   function (x) return x * x end)
}) end)

Calculator.consts = SomeParsers.identifier:flat_map(function (n) return fold({
  guard(n == "E",        2.7182818284590452),
  guard(n == "PI",       math.pi),
  guard(n == "LOG2E",    1.44269504088896340736),  -- log2(e)
  guard(n == "LOG10E",   0.434294481903251827651), -- log10(e)
  guard(n == "LN2",      0.693147180559945309417), -- ln(2)
  guard(n == "LN10",     2.30258509299404568402),  -- ln(10)
  guard(n == "PI_2",     1.57079632679489661923),  -- pi/2
  guard(n == "PI_4",     0.785398163397448309616), -- pi/4
  guard(n == "1_PI",     0.318309886183790671538), -- 1/pi
  guard(n == "2_PI",     0.636619772367581343076), -- 2/pi
  guard(n == "2_SQRTPI", 1.12837916709551257390),  -- 2/sqrt(pi)
  guard(n == "SQRT2",    1.41421356237309504880),  -- sqrt(2)
  guard(n == "SQRT1_2",  0.707106781186547524401)  -- 1/sqrt(2)
}) end)

Calculator.expr = function ()
  return SomeParsers.usign:flat_map(function (sgn)
    return Calculator.term():chainl1(Calculator.add:or_else(function () return Calculator.sub end), sgn == '-')
  end)
end

Calculator.term = function ()
  return Calculator.factor():chainl1(Calculator.mul:or_else(function () return Calculator.div end), false)
end

Calculator.factor = function ()
  return Calculator.factor0():chainr1(Calculator.pow)
end

Calculator.factor0 = function ()
  return Calculator.expr_in_brackets()
    :or_else(function () return Calculator.funcs:apply(function () return Calculator.expr_in_brackets() end) end)
    :or_else(function () return Calculator.consts end)
    :or_else(function () return SomeParsers.double end)
end

Calculator.expr_in_brackets = function ()
  return SomeParsers.between(SomeParsers.symbol('('), SomeParsers.symbol(')'), function () return Calculator.expr() end)
end

Calculator.calculate = function (s)
  return Calculator.expr():parse(s)
end

return Calculator
