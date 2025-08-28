local Parser = require("../src/parser")
local Maybe = require("../src/maybe")

function test(value, expected)
    print(expected == nil and value == expected or value ~= nil and value[1] == expected[1] and value[2] == expected[2])
end

function testParserPure()
    print("Testing Parser.pure...")
	test(Parser.pure(1):parse("abc").value, {1, "abc"})
	test(Parser.pure(1.0):parse("abc").value, {1.0, "abc"})
	test(Parser.pure("1"):parse("abc").value, {"1", "abc"})
end

function testParserMap()
    print("Testing Parser.map...")
	local fx = function (x)
        return tostring(x)
    end
	local fs = function (s)
        return tonumber(s)
    end

	test(Parser.pure(1):map(fx):parse("abc").value, {"1", "abc"})
	test(Parser.pure(1.0):map(fx):parse("abc").value, {"1.0", "abc"})
	test(Parser.pure("1"):map(fs):parse("abc").value, {1, "abc"})
	
	test(Parser.empty:map(fx):parse("abc").value, nil)
	test(Parser.empty:map(fx):parse("abc").value, nil)
	test(Parser.empty:map(fs):parse("abc").value, nil)
end

function testParserApply()
    print("Testing Parser.apply...")
	local psin = Parser.pure(math.sin)
	local fd = function ()
        return Parser.pure(1.0)
    end
	local nf = function ()
        return Parser.empty
    end

	test(psin:apply(fd):parse("abc").value, {math.sin(1.0), "abc"})
	test(psin:apply(nf):parse("abc").value, nil)
	test(Parser.empty:apply(fd):parse("abc").value, nil)
	test(Parser.empty:apply(nf):parse("abc").value, nil)
end

function testParserFlatMap()
    print("Testing Parser.flat_map...")
	local i1 = Parser.pure(1)
	local eat = function (x)
        return Parser:new(function (inp)
            return Maybe.some({tostring(x) .. inp, ""})
        end)
    end
	local cancel = function (x)
        return Parser:new(function (_)
            return Maybe.nothing
        end)
    end

	test(i1:flat_map(eat):parse("abc").value, {"1abc", ""})
	test(i1:flat_map(cancel):parse("abc").value, nil)
	test(Parser.empty:flat_map(eat):parse("abc").value, nil)
	test(Parser.empty:flat_map(cancel):parse("abc").value, nil)
end

testParserPure()
testParserMap()
testParserApply()
testParserFlatMap()
