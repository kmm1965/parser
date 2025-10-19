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

sub defObject {
  my ($n, $value) = @_;
  SomeParsers->name($n) >> sub { Parser->pure($value) }
}

sub funcs {
  fold(
    defObject("sin",   sub { sin(shift) }),
    defObject("cos",   sub { cos(shift) }),
    defObject("asin",  sub { asin(shift) }),
    defObject("acos",  sub { acos(shift) }),
    defObject("sinh",  sub { sinh(shift) }),
    defObject("cosh",  sub { cosh(shift) }),
    defObject("asinh", sub { asinh(shift) }),
    defObject("acosh", sub { acosh(shift) }),
    defObject("tan",   sub { tan(shift) }),
    defObject("log",   sub { log(shift) }),
    defObject("log10", sub { log(shift) / log(10) }),
    defObject("exp",   sub { exp(shift) }),
    defObject("sqrt",  sub { sqrt(shift) }),
    defObject("sqr",   sub { my $x = shift; $x * $x })
  )
}

sub consts {
  fold(
    defObject("E", 		    2.71828182845904523536),
    defObject("PI", 	    pi),
    defObject("LOG2E",    1.44269504088896340736),  # log2(e)
    defObject("LOG10E",   0.434294481903251827651), # log10(e)
    defObject("LN2", 	    0.693147180559945309417), # ln(2)
    defObject("LN10", 	  2.30258509299404568402),  # ln(10)
    defObject("PI_2", 	  1.57079632679489661923),  # pi/2
    defObject("PI_4", 	  0.785398163397448309616), # pi/4
    defObject("1_PI", 	  0.318309886183790671538), # 1/pi
    defObject("2_PI", 	  0.636619772367581343076), # 2/pi
    defObject("2_SQRTPI", 1.12837916709551257390),  # 2/sqrt(pi)
    defObject("SQRT2",    1.41421356237309504880),  # sqrt(2)
    defObject("SQRT1_2",  0.707106781186547524401)  # 1/sqrt(2)
  )
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
