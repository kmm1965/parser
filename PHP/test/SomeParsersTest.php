<?php
declare(strict_types=1);

namespace MonadParser\Parser\Test;

include __DIR__ . "/../src/Maybe/Maybe.php";
include __DIR__ . "/../src/Maybe/Constructor/Just.php";
include __DIR__ . "/../src/Maybe/Constructor/Nothing.php";
include __DIR__ . "/../src/Parser.php";
include __DIR__ . "/../src/SomeParsers.php";

use MonadParser\Maybe\Maybe;
use MonadParser\Parser\Parser;
use MonadParser\Parser\SomeParsers;

class SomeParsersTest extends \PHPUnit\Framework\TestCase
{
    public function test_anyChar()
    {
        $this->assertEquals(Parser::anyChar()->parse("abc"), Maybe::just(['a', "bc"]),
            'anyChar.parse("abc") is not equal to Just([\'a\', "bc"])');
        $this->assertEquals(Parser::anyChar()->parse(""), Maybe::nothing(),
            'anyChar.parse("") is not equal to Nothing');
    }

    public function test_satisfy()
    {
        $this->assertEquals(Parser::satisfy(function (string $c){ return $c == 'a'; })->parse("abc"), Maybe::just(['a', "bc"]),
            'satisfy(c==\'a\').parse("abc") is not equal to Just([\'a\', "bc"])');
        $this->assertEquals(Parser::satisfy(function (string $c){ return $c == 'z'; })->parse("abc"), Maybe::nothing(),
            'satisfy(c==\'z\').parse("abc") is not equal to Nothing');
    }

    public function test_char()
    {
        $this->assertEquals(SomeParsers::char_('a')->parse("abc"), Maybe::just(['a', "bc"]),
            'char(\'a\').parse("abc") is not equal to Just([\'a\', "bc"])');
        $this->assertEquals(SomeParsers::char_('z')->parse("abc"), Maybe::nothing(),
            'char(\'z\').parse("abc") is not equal to Nothing');
    }

    public function test_symbol()
    {
        $this->assertEquals(SomeParsers::symbol('+')->parse(" + abc"), Maybe::just(['+', "abc"]),
            'symbol(\'+\').parse(" + abc") is not equal to Just([\'+\', "abc"])');
        $this->assertEquals(SomeParsers::symbol('+')->parse("abc"), Maybe::nothing(),
            'char(\'+\').parse("abc") is not equal to Nothing');
    }

    public function test_empty_string()
    {
      $this->assertEquals(Parser::empty_string()->parse("abc"), Maybe::just(["", "abc"]),
        'empty_string.parse("abc") is not equal to Just(["", "abc"])');
    }

    public function test_optional()
    {
        $this->assertEquals(SomeParsers::optional_s(SomeParsers::char_('1'))->parse("1234"), Maybe::just(["1", "234"]),
            'optional_s(char(\'1\')).parse("1234") is not equal to Just(["1", "234"])');
        $this->assertEquals(SomeParsers::optional_s(SomeParsers::char_('1'))->parse("abc"), Maybe::just(["", "abc"]),
            'optional_s(char(\'1\')).parse("abc") is not equal to Just(["", "abc"])');
    }

    public function test_spaces()
    {
        $spaces = Parser::spaces();
        $this->assertEquals($spaces->parse("abc"), Maybe::just(["", "abc"]),
            'spaces.parse("abc") is not equal to Just(["", "abc"])');
        $this->assertEquals($spaces->parse("  abc"), Maybe::just(["  ", "abc"]),
            'spaces.parse("  abc") is not equal to Just(["  ", "abc"])');
    }

    public function test_alnum()
    {
        $alnum = SomeParsers::alnum();
        $this->assertEquals($alnum->parse("123abc"), Maybe::just(['1', "23abc"]),
            'alnum.parse("123abc") is not equal to Just([\'1\', "23abc"])');
        $this->assertEquals($alnum->parse("_123abc"), Maybe::just(['_', "123abc"]),
            'alnum.parse("_123abc") is not equal to Just([\'_\', "123abc"])');
        $this->assertEquals($alnum->parse("!@#$"), Maybe::nothing(),
            'alnum.parse("!@#$") is not equal to Nothing');

        $this->assertEquals($alnum->some()->parse("123abc"), Maybe::just(["123abc", ""]),
            'alnum.some().parse("123abc") is not equal to Just(["123abc", ""])');
    }

    public function test_name()
    {
        $psin = SomeParsers::name("sin");
      
        $this->assertEquals($psin->parse(" sin "), Maybe::just(["sin", ""]),
            'psin.parse(" sin ") is not equal to Just(["sin", ""])');
        $this->assertEquals($psin->parse(" sin (1.)"), Maybe::just(["sin", "(1.)"]),
            'psin.parse(" sin (1.)") is not equal to Just(["sin", "(1.)"])');
        $this->assertEquals($psin->parse("sinabc"), Maybe::nothing(),
            'psin.parse("sinabc") is not equal to Nothing');
    }

    public function test_digits()
    {
        $digits = SomeParsers::digits();
        
        $this->assertEquals($digits->parse("123abc"), Maybe::just(["123", "abc"]),
            'digits.parse("123abc") is not equal to Just(["123", "abc"])');
        $this->assertEquals($digits->parse("123  abc"), Maybe::just(["123", "  abc"]),
            'digits.parse("123  abc") is not equal to Just(["123", "  abc"])');
        $this->assertEquals($digits->parse("abc"), Maybe::just(["", "abc"]),
            'digits.parse("abc") is not equal to Just(["", "abc"])');
    }

    public function test_sign()
    {
        $sign = SomeParsers::sign();
        $usign = SomeParsers::usign();
        
        $this->assertEquals($sign->parse("abc"), Maybe::just(["", "abc"]),
            'sign.parse("abc") is not equal to Just(["", "abc"])');
        $this->assertEquals($sign->parse("+abc"), Maybe::just(["+", "abc"]),
            'sign.parse("+abc") is not equal to Just(["+", "abc"])');
        $this->assertEquals($sign->parse("-abc"), Maybe::just(["-", "abc"]),
            'sign.parse("-abc") is not equal to Just(["-", "abc"])');

        $this->assertEquals($usign->parse("abc"), Maybe::just(["", "abc"]),
            'usign.parse("abc") is not equal to Just(["", "abc"])');
        $this->assertEquals($usign->parse(" + abc"), Maybe::just(["+", "abc"]),
            'usign.parse("+abc") is not equal to Just(["+", "abc"])');
        $this->assertEquals($usign->parse(" - abc"), Maybe::just(["-", "abc"]),
            'usign.parse("-abc") is not equal to Just(["-", "abc"])');
    }

    public function test_double()
    {
        $double = SomeParsers::double_();
        
        $this->assertEquals($double->parse("1 abc"), Maybe::just([1, "abc"]),
            'double.parse("1 abc") is not equal to Just([1, "abc"])');
        $this->assertEquals($double->parse("1. abc"), Maybe::just([1, "abc"]),
            'double.parse("1. abc") is not equal to Just([1, "abc"])');
        $this->assertEquals($double->parse("1.23 abc"), Maybe::just([1.23, "abc"]),
            'double.parse("1.23 abc") is not equal to Just([1.23, "abc"])');
        $this->assertEquals($double->parse("-1.23 abc"), Maybe::just([-1.23, "abc"]),
            'double.parse("-1.23 abc") is not equal to Just([-1.23, "abc"])');
        $this->assertEquals($double->parse(".23 abc"), Maybe::just([0.23, "abc"]),
            'double.parse(".23 abc") is not equal to Just([0.23, "abc"])');
        $this->assertEquals($double->parse(" + 1.23 abc"), Maybe::nothing(),
            'double.parse(" + 1.23 abc") is not equal to Nothing');
        $this->assertEquals($double->parse("1.23e10abc"), Maybe::just([1.23e10, "abc"]),
            'double.parse("1.23e10abc") is not equal to Just([1.23e10, "abc"])');
        $this->assertEquals($double->parse("1.23e-10abc"), Maybe::just([1.23e-10, "abc"]),
            'double.parse("1.23e-10abc") is not equal to Just([1.23e-10, "abc"])');
        $this->assertEquals($double->parse("abc"), Maybe::nothing(),
            'double.parse("abc") is not equal to Nothing');
    }

    public function test_between()
    {
        $expr = Parser::between(SomeParsers::symbol('('), SomeParsers::symbol(')'),
            function(){ return SomeParsers::double_(); });
      
        $this->assertEquals($expr->parse("( 123 ) abc"), Maybe::just([123, "abc"]),
            'expr.parse(" ( 123 ) abc") is not equal to Just([123, "abc"])');
        $this->assertEquals($expr->parse("( 123 abc"), Maybe::nothing(),
            'expr.parse(" ( 123 abc") is not equal to Nothing');
        $this->assertEquals($expr->parse(" 123 ) abc"), Maybe::nothing(),
            'expr.parse(" 123 ) abc") is not equal to Nothing');
    }

    public function test_chainlr1()
    {
        $add = SomeParsers::symbol('+')->skip(function (){ return Parser::pure(function ($x, $y){ return $x + $y; }); });
        $sub = SomeParsers::symbol('-')->skip(function (){ return Parser::pure(function ($x, $y){ return $x - $y; }); });
        $pexpr = SomeParsers::double_()->chainl1($add->orElse($sub), false);
        $pow = SomeParsers::symbol('^')->skip(function (){ return Parser::pure(function ($x, $y){ return exp($y * log($x)); }); });

        $this->assertEquals($pexpr->parse("7abc"), Maybe::just([7, "abc"]),
            'pexpr.parse("7abc") is not equal to Just([7, "abc"])');
        $this->assertEquals($pexpr->parse("7 - 1 - 2 abc"), Maybe::just([4, "abc"]),
            'pexpr.parse("7 - 1 - 2 abc") is not equal to Just([4, "abc"])');
        $this->assertEquals($pexpr->parse("7 - 1 + 2 - 3 abc"), Maybe::just([5, "abc"]),
            'pexpr.parse("7 - 1 + 2 - 3 abc") is not equal to Just([5, "abc"])');
        $this->assertEquals($pexpr->parse("abc"), Maybe::nothing(),
            'pexpr.parse("abc") is not equal to Nothing');

        $this->assertEquals(SomeParsers::double_()->chainr1($pow)->parse("3 ^ 2 ^ 3 abc")->map(
            function ($pair){ return [round($pair[0]), $pair[1]]; }),
            Maybe::just([6561, "abc"]),
            'double.chainr1(pow).parse("3 ^ 2 ^ 3 abc") is not equal to Just([6561, "abc"])');
    }
}
