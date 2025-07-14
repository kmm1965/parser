<?php
declare(strict_types=1);

namespace MonadParser\Parser\Test;

include __DIR__ . "/../src/Maybe/Maybe.php";
include __DIR__ . "/../src/Maybe/Constructor/Just.php";
include __DIR__ . "/../src/Maybe/Constructor/Nothing.php";
include __DIR__ . "/../src/Parser.php";
include __DIR__ . "/../src/SomeParsers.php";
include __DIR__ . "/../src/Calculator.php";

use MonadParser\Maybe\Maybe;
use MonadParser\Parser\Parser;
use MonadParser\Parser\SomeParsers;
use MonadParser\Parser\Calculator;

class CalculatorTest extends \PHPUnit\Framework\TestCase
{
    public function test_calculator()
    {
        $c = new Calculator();

        $this->assertEquals($c->calc("72 - 7 - (1 - 2) * 3"), Maybe::just([68, ""]),
            'calc(\"72 - 7 - (1 - 2) * 3\") is not equal to Just([68, ""])');
        $this->assertEquals($c->calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"), Maybe::just([-8.889, ""]),
            'calc(\" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") is not equal to Just([-8.889, ""])');
        $this->assertEquals($c->calc("3^(1+1)^3")->map(function($pair){ return [round($pair[0]), $pair[1]]; }),
            Maybe::just([6561, ""]),
            'calc("3^(1+1)^3") is not equal to Just([6561, ""])');
        $this->assertEquals($c->calc("sin(1+1)"), Maybe::just([sin(2), ""]),
            'calc("sin(1+1)") is not equal to Just([sin(2), ""])');
        $this->assertEquals($c->calc("sqr(sin(2)) + sqr(cos(2))"), Maybe::just([1, ""]),
            'calc("sqr(sin(2)) + sqr(cos(2))") is not equal to Just([1, ""])');
        $this->assertEquals($c->calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"), Maybe::just([-0.3634085731426532, ""]),
            'calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 ) is not equal to Just([-0.3634085731426532, ""])');
        $this->assertEquals($c->calc("sqr(2 + 3)"), Maybe::just([25, ""]),
            'calc("sqr(2 + 3) is not equal to Just([25, ""])');
        $this->assertEquals($c->calc("sin(-PI/4)"), Maybe::just([-0.7071067811865475, ""]),
            'calc("sin(-PI/4) is not equal to Just([-0.7071067811865475, ""])');
        $this->assertEquals($c->calc(" E ^ PI"), Maybe::just([23.140692632779267, ""]),
            'calc(" E ^ PI is not equal to Just([23.140692632779267, ""])');
        $this->assertEquals($c->calc(" PI ^ E"), Maybe::just([22.45915771836104, ""]),
            'calc(" PI ^ E is not equal to Just([22.45915771836104, ""])');
    }
}
