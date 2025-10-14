<?php

declare(strict_types=1);

include __DIR__ . "/../src/Maybe/Maybe.php";
include __DIR__ . "/../src/Maybe/Constructor/Just.php";
include __DIR__ . "/../src/Maybe/Constructor/Nothing.php";
include __DIR__ . "/../src/Parser.php";
include __DIR__ . "/../src/SomeParsers.php";
include __DIR__ . "/../src/Calculator.php";

use MonadParser\Parser\Calculator;

$c = new Calculator();

print "Hello, world!\n";
print $c->calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")->get()[0] . "\n"; # -8.889
print $c->calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")->get()[0] . "\n";      # -0.3634085731426532
print $c->calc("sqr(sin(2)) + sqr(cos(1 + 1))")->get()[0] . "\n";         # 1.0
print $c->calc("3 ^ 2 ^ 3")->get()[0] . "\n";                             # 6561.0
print $c->calc(" E ^ PI ")->get()[0] . "\n";                              # 23.140692632779267
print $c->calc(" PI ^ E ")->get()[0] . "\n";                              # 22.45915771836104

