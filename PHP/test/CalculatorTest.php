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
use MonadParser\Parser\Calculator;

class CalculatorTest extends \PHPUnit\Framework\TestCase
{
    public function test_funcs()
    {
        $c = new Calculator();

        $this->assertEquals($c->calc("sin(2.0)"), Maybe::just([sin(2.0), ""]),
            'calc("sin(2.0)") is not equal to Just([sin(2.0), ""])');
        $this->assertEquals($c->calc("cos(2.0)"), Maybe::just([cos(2.0), ""]),
            'calc("cos(2.0)") is not equal to Just([cos(2.0), ""])');
        $this->assertEquals($c->calc("asin(0.5)"), Maybe::just([asin(0.5), ""]),
            'calc("asin(2.0)") is not equal to Just([asin(2.0), ""])');
        $this->assertEquals($c->calc("acos(0.5)"), Maybe::just([acos(0.5), ""]),
            'calc("acos(2.0)") is not equal to Just([acos(2.0), ""])');
        $this->assertEquals($c->calc("sinh(2.0)"), Maybe::just([sinh(2.0), ""]),
            'calc("sinh(2.0)") is not equal to Just([sinh(2.0), ""])');
        $this->assertEquals($c->calc("cosh(2.0)"), Maybe::just([cosh(2.0), ""]),
            'calc("cosh(2.0)") is not equal to Just([cosh(2.0), ""])');
        $this->assertEquals($c->calc("asinh(2.0)"), Maybe::just([asinh(2.0), ""]),
            'calc("asinh(2.0)") is not equal to Just([asinh(2.0), ""])');
        $this->assertEquals($c->calc("acosh(2.0)"), Maybe::just([acosh(2.0), ""]),
            'calc("acosh(2.0)") is not equal to Just([acosh(2.0), ""])');
        $this->assertEquals($c->calc("tan(2.0)"), Maybe::just([tan(2.0), ""]),
            'calc("tan(2.0)") is not equal to Just([tan(2.0), ""])');
        $this->assertEquals($c->calc("log(2.0)"), Maybe::just([log(2.0), ""]),
            'calc("log(2.0)") is not equal to Just([log(2.0), ""])');
        $this->assertEquals($c->calc("log10(2.0)"), Maybe::just([log10(2.0), ""]),
            'calc("log10(2.0)") is not equal to Just([log10(2.0), ""])');
        $this->assertEquals($c->calc("exp(2.0)"), Maybe::just([exp(2.0), ""]),
            'calc("exp(2.0)") is not equal to Just([exp(2.0), ""])');
        $this->assertEquals($c->calc("sqrt(2.0)"), Maybe::just([sqrt(2.0), ""]),
            'calc("sqrt(2.0)") is not equal to Just([sqrt(2.0), ""])');
        $this->assertEquals($c->calc("sqr(2.0)"), Maybe::just([4.0, ""]),
            'calc("sqr(2.0)") is not equal to Just([4.0, ""])');
    }

    public function test_consts()
    {
        $c = new Calculator();
        $pi = 3.14159265358979323846;

        $this->assertEquals($c->calc("E"), Maybe::just([2.71828182845904523536, ""]),
            'calc("E") is not equal to Just([2.71828182845904523536, ""])');
        $this->assertEquals($c->calc("LOG2E"), Maybe::just([1 / log(2.0), ""]),
            'calc("LOG2E") is not equal to Just([1/log(2.0), ""])');
        $this->assertEquals($c->calc("LOG10E"), Maybe::just([0.434294481903251827651, ""]),
            'calc("LOG10E") is not equal to Just([0.434294481903251827651, ""])');
        //$this->assertEquals($c->calc("LOG10E"), Maybe::just([1 / log(10.0), ""]),
        //    'calc("LOG10E") is not equal to Just([1/log(10.0), ""])');
        $this->assertEquals($c->calc("LN2"), Maybe::just([log(2.0), ""]),
            'calc("LN2") is not equal to Just([log(2.0), ""])');
        $this->assertEquals($c->calc("LN10"), Maybe::just([log(10), ""]),
            'calc("LN10") is not equal to Just([log(10.0), ""])');
        $this->assertEquals($c->calc("PI"), Maybe::just([$pi, ""]),
            'calc("PI") is not equal to Just([pi, ""])');
        $this->assertEquals($c->calc("PI_2"), Maybe::just([$pi / 2, ""]),
            'calc("PI_2") is not equal to Just([pi/2, ""])');
        $this->assertEquals($c->calc("PI_4"), Maybe::just([$pi / 4, ""]),
            'calc("PI_4") is not equal to Just([pi/4, ""])');
        $this->assertEquals($c->calc("1_PI"), Maybe::just([1 / $pi, ""]),
            'calc("1_PI") is not equal to Just([1/pi, ""])');
        $this->assertEquals($c->calc("2_PI"), Maybe::just([2 / $pi, ""]),
            'calc("2_PI") is not equal to Just([2/pi, ""])');
        $this->assertEquals($c->calc("2_SQRTPI"), Maybe::just([2 / sqrt($pi), ""]),
            'calc("2_SQRTPI") is not equal to Just([2/sqrt(pi), ""])');
        $this->assertEquals($c->calc("SQRT2"), Maybe::just([sqrt(2), ""]),
            'calc("SQRT2") is not equal to Just([sqrt(2), ""])');
        $this->assertEquals($c->calc("SQRT1_2"), Maybe::just([sqrt(0.5), ""]),
            'calc("SQRT1_2") is not equal to Just([1/sqrt(2), ""])');
    }

    public function test_calculator()
    {
        $c = new Calculator();

        $this->assertEquals($c->calc("72 - 7 - (1 - 2) * 3"), Maybe::just([68, ""]),
            'calc("72 - 7 - (1 - 2) * 3\") is not equal to Just([68, ""])');
        $this->assertEquals($c->calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"), Maybe::just([-8.889, ""]),
            'calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") is not equal to Just([-8.889, ""])');
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
