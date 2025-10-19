<?php
declare(strict_types=1);

namespace MonadParser\Parser\Test;

include __DIR__ . "/../src/Parser.php";
include __DIR__ . "/../src/Maybe/Maybe.php";
include __DIR__ . "/../src/Maybe/Constructor/Just.php";
include __DIR__ . "/../src/Maybe/Constructor/Nothing.php";

use MonadParser\Parser\Parser;
use MonadParser\Maybe\Maybe;

class ParserTest extends \PHPUnit\Framework\TestCase
{
    public function testParserPure()
    {
        $this->assertEquals(Parser::pure(1)->parse("abc"), Maybe::just([1, "abc"]), 'pure(1)->parse("abc") is not equal to Just([1, "abc"])');
        $this->assertEquals(Parser::pure("1")->parse("abc"), Maybe::just(["1", "abc"]), 'pure("1")->parse("abc") is not equal to Just(["1", "abc"])');
    }

    public function testParserMap()
    {
        $fi = function (int $i) : string { return strval($i); };
        $fs = function (string $s) : int { return intval($s); };

        $this->assertEquals(Parser::pure(1)->map($fi)->parse("abc"), Maybe::just(["1", "abc"]), 'pure(1).map(fi)->parse("abc") is not equal to Just(["1", "abc"])');
        $this->assertEquals(Parser::pure("1")->map($fs)->parse("abc"), Maybe::just([1, "abc"]), 'pure("1").map(fs)->parse("abc") is not equal to Just([1, "abc"])');

        $this->assertEquals(Parser::empty()->map($fi)->parse("abc"), Maybe::nothing(), 'empty.map(fi)->parse("abc") is not equal to Nothing');
        $this->assertEquals(Parser::empty()->map($fs)->parse("abc"), Maybe::nothing(), 'empty.map(fs)->parse("abc") is not equal to Nothing');
    }

    public function testParserAp()
    {
        $psin = Parser::pure(function ($x){ return sin($x); });
        $fd = function (){ return Parser::pure(1); };
        $nf = function (){ return Parser::empty(); };

        $this->assertEquals($psin->ap($fd)->parse("abc"), Maybe::just([sin(1), "abc"]), 'pure(sin).ap(pure(1))->parse("abc") is not equal to Just([sin(1), "abc"])');
        $this->assertEquals($psin->ap($nf)->parse("abc"), Maybe::nothing(), 'pure(sin).ap(empty)->parse("abc") is not equal to Nothing');
        $this->assertEquals(Parser::empty()->ap($fd)->parse("abc"), Maybe::nothing(), 'empty.ap(pure(1))->parse("abc") is not equal to Nothing');
        $this->assertEquals(Parser::empty()->ap($nf)->parse("abc"), Maybe::nothing(), 'empty.ap(empty)->parse("abc") is not equal to Nothing');
    }

    public function testParserFlatMap()
    {
        $i1 = Parser::pure(1);
        $iempty = Parser::empty();
        $eat = function ($x){
            return new Parser(function (string $inp) use($x){
                return Maybe::just([strval($x) . $inp, ""]);
            });
        };
        $cancel = function ($_){
            return new Parser(function ($_){
                return Maybe::nothing();
            });
        };

        $this->assertEquals($i1->flatMap($eat)->parse("abc"), Maybe::just(["1abc", ""]), 'i1.flatMap(eat)->parse("abc") is not equal to Just(["1abc", ""])');
        $this->assertEquals($i1->flatMap($cancel)->parse("abc"), Maybe::nothing(), 'i1.>flatMap(cancel)->parse("abc") is not equal to Nothing');
        $this->assertEquals($iempty->flatMap($eat)->parse("abc"), Maybe::nothing(), 'iempty->flatMap(eat)->parse("abc") is not equal to Nothing');
        $this->assertEquals($iempty->flatMap($cancel)->parse("abc"), Maybe::nothing(), 'iempty->flatMap(cancel)->parse("abc") is not equal to Nothing');
    }
}
