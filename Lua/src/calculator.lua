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

function defObject(n, value)
  return SomeParsers.name(n):skip(function () return Parser.pure(value) end)
end

Calculator.funcs = fold({
  defObject("sin",   function (x) return math.sin(x) end),
  defObject("cos",   function (x) return math.cos(x) end),
  defObject("asin",  function (x) return math.asin(x) end),
  defObject("acos",  function (x) return math.acos(x) end),
  defObject("sinh",  function (x) return math.sinh(x) end),
  defObject("cosh",  function (x) return math.cosh(x) end),
  defObject("tan",   function (x) return math.tan(x) end),
  defObject("log",   function (x) return math.log(x) end),
  defObject("log10", function (x) return math.log10(x) end),
  defObject("exp",   function (x) return math.exp(x) end),
  defObject("sqrt",  function (x) return math.sqrt(x) end),
  defObject("sqr",   function (x) return x * x end)
})

Calculator.consts = fold({
  defObject("E",        2.7182818284590452),
  defObject("PI",       math.pi),
  defObject("LOG2E",    1.44269504088896340736),  -- log2(e)
  defObject("LOG10E",   0.434294481903251827651), -- log10(e)
  defObject("LN2",      0.693147180559945309417), -- ln(2)
  defObject("LN10",     2.30258509299404568402),  -- ln(10)
  defObject("PI_2",     1.57079632679489661923),  -- pi/2
  defObject("PI_4",     0.785398163397448309616), -- pi/4
  defObject("1_PI",     0.318309886183790671538), -- 1/pi
  defObject("2_PI",     0.636619772367581343076), -- 2/pi
  defObject("2_SQRTPI", 1.12837916709551257390),  -- 2/sqrt(pi)
  defObject("SQRT2",    1.41421356237309504880),  -- sqrt(2)
  defObject("SQRT1_2",  0.707106781186547524401)  -- 1/sqrt(2)
})

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
