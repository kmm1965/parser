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

#my $add = op2('+', sub { my ($x, $y) = @_; $x + $y });
#my $sub = op2('-', sub { my ($x, $y) = @_; $x - $y });
#my $mul = op2('*', sub { my ($x, $y) = @_; $x * $y });
#my $div = op2('/', sub { my ($x, $y) = @_; $x / $y });
#my $pow = op2('^', sub { my ($x, $y) = @_; exp($y * log($x)) });

my $add = op2('+', sub { shift() + shift });
my $sub = op2('-', sub { shift() - shift });
my $mul = op2('*', sub { shift() * shift });
my $div = op2('/', sub { shift() / shift });
my $pow = op2('^', sub { exp(log(shift) * shift) });

sub defObject {
  my ($n, $value) = @_;
  SomeParsers->name($n) >> sub { Parser->pure($value) }
}

sub funcs {
       (defObject("sin",   sub { sin(shift) }) |
  sub { defObject("cos",   sub { cos(shift) }) } |
  sub { defObject("asin",  sub { asin(shift) }) } |
  sub { defObject("acos",  sub { acos(shift) }) } |
  sub { defObject("sinh",  sub { sinh(shift) }) } |
  sub { defObject("cosh",  sub { cosh(shift) }) } |
  sub { defObject("tan",   sub { tan(shift) }) } |
  sub { defObject("log",   sub { log(shift) }) } |
  sub { defObject("log10", sub { log(shift) / log(10) }) } |
  sub { defObject("exp",   sub { exp(shift) }) } |
  sub { defObject("sqrt",  sub { sqrt(shift) }) } |
  sub { defObject("sqr",   sub { my $x = shift; $x * $x }) }
  )->token
}

sub consts {
       (defObject("E", 		    2.7182818284590452)   | # e 2.7182818284590452353
  sub { defObject("PI", 	    pi)                 } | # pi 3.14159265358979323846
  sub { defObject("LOG2E",    1.4426950408889634) } | # log2(e)
  sub { defObject("LOG10E",   0.4342944819032518) } | # log10(e)
  sub { defObject("LN2", 	    0.6931471805599453) } | # ln(2)
  sub { defObject("LN10", 	  2.302585092994046)  } | # ln(10)
  sub { defObject("PI_2", 	  1.5707963267948966) } | # pi/2
  sub { defObject("PI_4", 	  0.7853981633974483) } | # pi/4
  sub { defObject("1_PI", 	  0.3183098861837907) } | # 1/pi
  sub { defObject("2_PI", 	  0.6366197723675814) } | # 2/pi
  sub { defObject("2_SQRTPI", 1.1283791670955126) } | # 2/sqrt(pi)
  sub { defObject("SQRT2",    1.4142135623730951) } | # sqrt(2)
  sub { defObject("SQRT1_2",  0.7071067811865476) }   # 1/sqrt(2)
  )->token
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
