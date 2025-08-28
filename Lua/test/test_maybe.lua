local Maybe = require("../src/maybe")

local some = Maybe.some
local nothing = Maybe.nothing

-- Test Maybe:map
print(some(1.0):map(math.sin).value == math.sin(1.0))
print(nothing:map(math.sin).value == nil)
print(some(1):map(tostring).value == "1")
print(nothing:map(tostring).value == nil)

-- Test Maybe:flat_map
function safe_sqrt(x)
    return x < 0 and nothing or some(math.sqrt(x))
end

function safe_log(x)
    return x <= 0 and nothing or some(math.log(x))
end

print(safe_sqrt(2.0).value == math.sqrt(2.0))
print(safe_sqrt(0.0).value == 0)
print(safe_sqrt(-2.0).value == nil)

print(safe_log(2.0).value == math.log(2.0))
print(safe_log(0.0).value == nil)
print(safe_log(-2.0).value == nil)

print(some(2.0):flat_map(safe_sqrt).value == math.sqrt(2.0))
print(some(0.0):flat_map(safe_sqrt).value == 0)
print(some(-2.0):flat_map(safe_sqrt).value == nil)

print(some(2.0):flat_map(safe_sqrt):flat_map(safe_log).value == math.log(math.sqrt(2.0)))
print(some(0.0):flat_map(safe_sqrt):flat_map(safe_log).value == nil)
print(some(-2.0):flat_map(safe_sqrt):flat_map(safe_log).value == nil)

function toString(i)
    return i % 2 == 1 and nothing or some(tostring(i))
end

print(some(2):flat_map(toString).value == "2")
print(some(1):flat_map(toString).value == nil)
print(nothing:flat_map(toString).value == nil)
