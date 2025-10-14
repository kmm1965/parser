local Maybe = require("../src/maybe")

local Parser = {}

function Parser:new(unp)
  local obj = {}
    obj.unp = unp

  function obj:parse(inp)
    return self.unp(inp)
  end

  function obj:map(f)
    return self:flat_map(function (a) return Parser.pure(f(a)) end)
  end

  function obj:flat_map(f)
    return Parser:new(function (inp)
      return self:parse(inp):flat_map(function (p)
        return f(p[1]):parse(p[2])
      end)
    end)
  end

  function obj:skip(pf)
    return self:flat_map(function (_)
      return pf()
    end)
  end

  function obj:or_else(f)
    return Parser:new(function (inp)
      return self:parse(inp):or_else(function () return f():parse(inp) end)
    end)
  end

  function obj:apply(pf)
    return self:flat_map(function (f) return pf():map(f) end)
  end

  function obj:some()
    return self:map(function (c)
      return function (s) return c .. s end
    end):apply(function () return self:many() end)
  end

  function obj:many()
    return self:some():or_else(function () return Parser.empty_string end)
  end

  function obj:rest_l(op, a)
    return Parser.rest(function () return self end,
      function (b) return self:rest_l(op, b) end,
      op, a)
  end

  function obj:rest_r(op, a)
    return Parser.rest(function () return self:chainr1(op) end, Parser.pure, op, a)
  end

  function obj:chainl1(op, negate_first)
    return self:flat_map(function (a)
      return self:rest_l(op, negate_first and -a or a)
    end)
  end

  function obj:chainr1(op)
    return self:flat_map(function (a) return self:rest_r(op, a) end)
  end

  setmetatable(obj, { __index = self }) -- Устанавливаем мета-таблицу для наследования
  return obj
end

function Parser.pure(value)
  return Parser:new(function (inp)
    return Maybe.some({value, inp})
  end)
end

Parser.empty = Parser:new(function (_)
  return Maybe.nothing
end)

Parser.empty_string = Parser.pure("")

Parser.rest = function (fval, ff, op, a)
  return op:flat_map(function (f)
    return fval():flat_map(function (b) return ff(f(a, b)) end)
  end):or_else(function () return Parser.pure(a) end)
end

return Parser
