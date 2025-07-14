<?php
declare(strict_types=1);

namespace MonadParser\Maybe\Test;

include __DIR__ . "/../src/Maybe/Maybe.php";
include __DIR__ . "/../src/Maybe/Constructor/Just.php";
include __DIR__ . "/../src/Maybe/Constructor/Nothing.php";

use MonadParser\Maybe\Maybe;

class MaybeTest extends \PHPUnit\Framework\TestCase
{
    public function testMaybeMap()
    {
        $fsin = function ($x){ return sin($x); };
        
        $this->assertEquals(Maybe::just(1)->map($fsin), Maybe::just(sin(1)), 'Just(1).map(sin) is not equal to Just(sin(1))');
        $this->assertEquals(Maybe::nothing()->map($fsin), Maybe::nothing(), 'Nothing.map(sin) is not equal to Nothing');
    }
    
    public function test_maybe_flatMap()
    {
        $safe_sqrt = function ($x){ return $x >= 0 ? Maybe::just(sqrt($x)) : Maybe::nothing(); };
        $safe_log  = function ($x){ return $x >  0 ? Maybe::just(log ($x)) : Maybe::nothing(); };

        $this->assertEquals($safe_sqrt(2), Maybe::just(sqrt(2)), 'safe_sqrt(2) is not equal to Just(sqrt(2))');
        $this->assertEquals($safe_sqrt(0), Maybe::just(0), 'safe_sqrt(0) is not equal to Just(0)');
        $this->assertEquals($safe_sqrt(-2), Maybe::nothing(), 'safe_sqrt(-2) is not equal to Nothing');

        $this->assertEquals($safe_log(2), Maybe::just(log(2)), 'safe_log(2) is not equal to Just(log(2))');
        $this->assertEquals($safe_log(0), Maybe::nothing(), 'safe_log(0) is not equal to Nothing');
        $this->assertEquals($safe_log(-2), Maybe::nothing(), 'safe_log(-2) is not equal to Nothing');

        $this->assertEquals(Maybe::just(2)->flatMap($safe_sqrt), Maybe::just(sqrt(2)), 'Just(2).flatMap(safe_sqrt) is not equal to Just(sqrt(2))');
        $this->assertEquals(Maybe::just(0)->flatMap($safe_sqrt), Maybe::just(0), 'Just(0).flatMap(safe_sqrt) is not equal to Just(0)');
        $this->assertEquals(Maybe::just(-2)->flatMap($safe_sqrt), Maybe::nothing(), 'Just(-2).flatMap(safe_sqrt) is not equal to Nothing');

        $this->assertEquals($safe_sqrt(2)->flatMap($safe_log), Maybe::just(log(sqrt(2))), 'safe_sqrt(2).flatMap(safe_log) is not equal to Just(log(sqrt(2)))');
        $this->assertEquals($safe_sqrt(-2)->flatMap($safe_log), Maybe::nothing(), 'safe_sqrt(-2).flatMap(safe_log) is not equal to Nothing');
        $this->assertEquals($safe_sqrt(0)->flatMap($safe_log), Maybe::nothing(), 'safe_sqrt(0).flatMap(safe_log) is not equal to Nothing');
        $this->assertEquals(Maybe::nothing()->flatMap($safe_log), Maybe::nothing(), 'Nothing.flatMap(safe_log) is not equal to Nothing');

        $to_string = function($i){ return $i % 2 == 0 ? Maybe::just(strval($i)) : Maybe::nothing(); };

        $this->assertEquals(Maybe::just(2)->flatMap($to_string), Maybe::just("2"), 'Just(2).flatMap(to_string) is not equal to Just("2")');
        $this->assertEquals(Maybe::just(1)->flatMap($to_string), Maybe::nothing(), 'Just(1).flatMap(to_string) is not equal to Nothing');
        $this->assertEquals(Maybe::nothing()->flatMap($to_string), Maybe::nothing(), 'Nothing.flatMap(to_string) is not equal to Nothing');
    }
    
    public function test_maybe_ap()
    {
        $psin = Maybe::just(function ($x){ return sin($x); });

        $this->assertEquals($psin->ap(Maybe::just(2)), Maybe::just(sin(2)), 'Just(sin)->ap(Just(2)) is not equal to Just(sin(2))');
        $this->assertEquals($psin->ap(Maybe::nothing()), Maybe::nothing(), 'Just(sin)->ap(Nothing) is not equal to Nothing');
        $this->assertEquals(Maybe::nothing()->ap(Maybe::just(2)), Maybe::nothing(), 'Just(sin)->ap(Just(2)) is not equal to Nothing');
        $this->assertEquals(Maybe::nothing()->ap(Maybe::nothing()), Maybe::nothing(), 'Just(sin)->ap(Nothing) is not equal to Nothing');
    }
}
