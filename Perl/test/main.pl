require "./src/calculator.pl";

use strict;
use warnings;

sub calc {
  Calculator->calculate(shift);
}

print calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") . "\n"; # -8.889
print calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )") . "\n";      # -0.363408573142653
print calc("sqr(sin(2)) + sqr(cos(1 + 1))") . "\n";         # 1.0
print calc("3 ^ 2 ^ 3") . "\n";                             # 6561.0
print calc(" E ^ PI ") . "\n";                              # 23.1406926327793
print calc(" PI ^ E ") . "\n";                              # 22.459157718361
