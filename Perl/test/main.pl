require "./src/calculator.pl";

use strict;
use warnings;

sub calc {
  Calculator->calculate(shift);
}

print calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") . "\n"; # -8.889
print calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )") . "\n";      # -0.3634085731426532
print calc("sqr(sin(2)) + sqr(cos(1 + 1))") . "\n";         # 1.0
print calc("3 ^ 2 ^ 3") . "\n";                             # 6561.0
print calc(" E ^ PI ") . "\n";                              # 23.140692632779267
print calc(" PI ^ E ") . "\n";                              # 22.45915771836104
