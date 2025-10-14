local Maybe = {}

function Maybe:new(value)
  local obj = {}
    obj.value = value

  function obj:map(f)
    return self:flat_map(function (a) return Maybe:new(f(a)) end)
  end

  function obj:flat_map(f)
    return self.value == nil and self or f(self.value)
  end

  function obj:or_else(f)
    return self.value ~= nil and self or f()
  end

  setmetatable(obj, { __index = self }) -- Устанавливаем мета-таблицу для наследования
  return obj
end

function Maybe.some(value)
  return Maybe:new(value)
end

Maybe.nothing = Maybe:new(nil)

return Maybe
