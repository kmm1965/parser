<?php
declare(strict_types=1);

include "Maybe/Maybe.php";
include "Maybe/Constructor/Just.php";
include "Maybe/Constructor/Nothing.php";
include "Parser.php";
include "SomeParsers.php";
include "Calculator.php";

use MonadParser\Parser\Calculator;

$c = new Calculator();

echo implode(',', $c->calc("72 - 7 - (1 - 2) * 3")->get()) . "\n";                  // 68.0
echo implode(',', $c->calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")->get()) . "\n"; // -8.889
echo implode(',', $c->calc("3^(1+1)^3")->get()) . "\n";                             // 6561
echo implode(',', $c->calc("sin(1+1)")->get()) . "\n";                              // 0.90929742682568
echo implode(',', $c->calc("sqr(sin(2)) + sqr(cos(2))")->get()) . "\n";             // 1
echo implode(',', $c->calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")->get()) . "\n";      // -0.36340857314265
echo implode(',', $c->calc("sqr(2 + 3)")->get()) . "\n";                            // 25
echo implode(',', $c->calc("sin(-PI/4)")->get()) . "\n";                            // -0.70710678118655
echo implode(',', $c->calc(" E ^ PI")->get()) . "\n";                               // 23.140692632779
echo implode(',', $c->calc(" PI ^ E")->get()) . "\n";                               // 22.459157718361
