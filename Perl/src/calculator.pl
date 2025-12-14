package Calculator;

require "./src/some_parsers.pl";

use List::Util qw(reduce);
use Math::Trig;

use strict;
use warnings;

sub op2 {
  my ($c, $f) = @_;
  SomeParsers->symbol($c) >> sub { Parser->pure($f) }
}

my $add = op2('+', sub { shift() + shift });
my $sub = op2('-', sub { shift() - shift });
my $mul = op2('*', sub { shift() * shift });
my $div = op2('/', sub { shift() / shift });
my $pow = op2('^', sub { exp(log(shift) * shift) });

sub fold {
  (reduce { $a->orElseVal($b) } @_)->token
}

sub guard {
  my ($b, $value) = @_;
  $b ? Parser->pure($value) : Parser->empty
}

sub funcs {
  SomeParsers->identifier->andThen(sub {
    my $n = shift;
    fold(
      guard($n eq "sin",   sub { sin(shift) }),
      guard($n eq "cos",   sub { cos(shift) }),
      guard($n eq "asin",  sub { asin(shift) }),
      guard($n eq "acos",  sub { acos(shift) }),
      guard($n eq "sinh",  sub { sinh(shift) }),
      guard($n eq "cosh",  sub { cosh(shift) }),
      guard($n eq "asinh", sub { asinh(shift) }),
      guard($n eq "acosh", sub { acosh(shift) }),
      guard($n eq "tan",   sub { tan(shift) }),
      guard($n eq "log",   sub { log(shift) }),
      guard($n eq "log10", sub { log(shift) / log(10) }),
      guard($n eq "exp",   sub { exp(shift) }),
      guard($n eq "sqrt",  sub { sqrt(shift) }),
      guard($n eq "sqr",   sub { my $x = shift; $x * $x })
    )
  })
}

sub consts {
  SomeParsers->identifier->andThen(sub {
    my $n = shift;
    fold(
      guard($n eq "E", 		    2.71828182845904523536),
      guard($n eq "PI", 	    pi),
      guard($n eq "LOG2E",    1.44269504088896340736),  # log2(e)
      guard($n eq "LOG10E",   0.434294481903251827651), # log10(e)
      guard($n eq "LN2", 	    0.693147180559945309417), # ln(2)
      guard($n eq "LN10", 	  2.30258509299404568402),  # ln(10)
      guard($n eq "PI_2", 	  1.57079632679489661923),  # pi/2
      guard($n eq "PI_4", 	  0.785398163397448309616), # pi/4
      guard($n eq "1_PI", 	  0.318309886183790671538), # 1/pi
      guard($n eq "2_PI", 	  0.636619772367581343076), # 2/pi
      guard($n eq "2_SQRTPI", 1.12837916709551257390),  # 2/sqrt(pi)
      guard($n eq "SQRT2",    1.41421356237309504880),  # sqrt(2)
      guard($n eq "SQRT1_2",  0.707106781186547524401)  # 1/sqrt(2)
    )
  })
}

sub expr {
  SomeParsers->usign->andThen(sub {
    term()->chainl1($add | sub { $sub }, shift eq '-')
  })
}

sub term {
  factor()->chainl1($mul | sub { $div }, 0)
}

sub factor {
    factor0()->chainr1($pow)
}

sub factor0 {
  expr_in_brackets() |
  sub { funcs->apply(sub { expr_in_brackets() }) } |
  sub { consts } |
  sub { SomeParsers->double }
}

sub expr_in_brackets {
  Parser->between(SomeParsers->symbol('('), SomeParsers->symbol(')'), sub { expr })
}

sub calculate {
  my ($class, $s) = @_;
  expr->parse($s)
}
