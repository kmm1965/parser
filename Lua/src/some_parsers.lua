local Parser = require("../src/parser")
local Maybe = require("../src/maybe")

local SomeParsers = {}

SomeParsers.anyChar = Parser:new(function (inp)
  return #inp == 0 and Maybe.nothing or Maybe.some({string.sub(inp, 1, 1), string.sub(inp, 2)})
end)

SomeParsers.satisfy = function (pred)
  return SomeParsers.anyChar:flat_map(function (c)
    return pred(c) and Parser.pure(c) or Parser.empty
  end)
end

SomeParsers.char = function (c)
  return SomeParsers.satisfy(function (x) return x == c end)
end

SomeParsers.between = function (open, close, fp)
  return open:skip(function ()
    return fp():flat_map(function (x)
      return close:skip(function () return Parser.pure(x) end)
    end)
  end)
end

SomeParsers.spaces = SomeParsers.satisfy(function (c) return string.match(c, "%s") end):many()

SomeParsers.token = function (p)
  return SomeParsers.between(SomeParsers.spaces, SomeParsers.spaces, function () return p end)
end

SomeParsers.symbol = function (c)
  return SomeParsers.token(SomeParsers.char(c))
end

SomeParsers.alnum = SomeParsers.satisfy(function (c)
  return string.match(c, "%w") or c == '_'
end)

SomeParsers.name = function (n)
  return SomeParsers.token(SomeParsers.alnum:some():flat_map(function (n2)
    return n2 == n and Parser.pure(n) or Parser.empty
  end))
end

SomeParsers.optional_s = function (p)
  return p:or_else(function () return Parser.empty_string end)
end

SomeParsers.digit = SomeParsers.satisfy(function (c)
  return string.match(c, "%d")
end)

SomeParsers.digits = SomeParsers.digit:many()

SomeParsers.sign = SomeParsers.optional_s(SomeParsers.char('+'):or_else(function () return SomeParsers.char('-') end))

-- Unary sign
SomeParsers.usign = SomeParsers.token(SomeParsers.sign)

SomeParsers.double = SomeParsers.token(SomeParsers.digits:flat_map(function
  (int_part) return SomeParsers.optional_s(SomeParsers.char('.'):skip(function () return SomeParsers.digits end)):flat_map(function
  (frac_part) return SomeParsers.optional_s(SomeParsers.char('e'):or_else(function () return SomeParsers.char('E') end):skip(
    function () return SomeParsers.sign end):flat_map(function
	  (exp_sign) return SomeParsers.digit:some():flat_map(function
      (exp_digits) return Parser.pure(exp_sign .. exp_digits) end) end)):flat_map(function
  (exp_part) return (#int_part > 0 or #frac_part > 0) and Parser.pure(tonumber(int_part ..
      (#frac_part > 0 and '.' .. frac_part or "") ..
      (#exp_part > 0 and 'e' .. exp_part or "" )))
    or Parser.empty end) end) end))

return SomeParsers
