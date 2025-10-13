require "./src/some_parsers.pl";

use strict;
use warnings;
use Test::More; #tests => 2; # Declare the number of expected tests

sub test_char {
  is(SomeParsers->char('a')->parse("abc"), Maybe->Just(Pair->new('a', "bc")),
    "char('a')->parse(\"abc\") should be equal to Just(('a', \"bc\")");
  is(SomeParsers->char('z')->parse("abc"), Maybe->Nothing,
    "char('z')->parse(\"abc\") should be equal to Nothing");
  is(SomeParsers->char('a')->parse(""), Maybe->Nothing,
    "char('a')->parse(\"\") should be equal to Nothing");
}

sub test_symbol {
  is(SomeParsers->symbol('+')->parse(" + abc"), Maybe->Just(Pair->new('+', "abc")),
    "symbol('+')->parse(\" + abc\") should be equal to Just(('+', \"abc\"))");
  is(SomeParsers->symbol('+')->parse("abc"), Maybe->Nothing,
    "symbol('+')->parse(\"abc\") should be equal to Nothing");
}

sub test_alnum {
  is(SomeParsers->alnum->parse("123abc"), Maybe->Just(Pair->new('1', "23abc")),
    "alnum->parse(\"123abc\") should be equal to Just(('1', \"23abc\"))");
  is(SomeParsers->alnum->parse("abc"), Maybe->Just(Pair->new('a', "bc")),
    "alnum->parse(\"abc\") should be equal to Just(('a', \"bc\"))");
  is(SomeParsers->alnum->parse("_123abc"), Maybe->Just(Pair->new('_', "123abc")),
    "alnum->parse(\"_123abc\") should be equal to Just(('_', \"123abc\"))");
  is(SomeParsers->alnum->parse("!@#"), Maybe->Nothing,
    "alnum->parse(\"!@#\") should be equal to Nothing");
}

sub test_sign {
  is(SomeParsers->sign->parse("abc"), Maybe->Just(Pair->new("", "abc")),
    "sign->parse(\"abc\") should be equal to Just((\"\", \"abc\"))");
  is(SomeParsers->sign->parse("+abc"), Maybe->Just(Pair->new("+", "abc")),
    "sign->parse(\"+abc\") should be equal to Just((\"+\", \"abc\"))");
  is(SomeParsers->sign->parse("-abc"), Maybe->Just(Pair->new("-", "abc")),
    "sign->parse(\"-abc\") should be equal to Just((\"-\", \"abc\"))");

  is(SomeParsers->usign->parse("abc"), Maybe->Just(Pair->new("", "abc")),
    "usign->parse(\"abc\") should be equal to Just((\"\", \"abc\"))");
  is(SomeParsers->usign->parse(" + abc"), Maybe->Just(Pair->new("+", "abc")),
    "usign->parse(\" + abc\") should be equal to Just((\"+\", \"abc\"))");
  is(SomeParsers->usign->parse(" - abc"), Maybe->Just(Pair->new("-", "abc")),
    "usign->parse(\" - abc\") should be equal to Just((\"-\", \"abc\"))");
}

sub test_digits {
  is(SomeParsers->digits->parse("123abc"), Maybe->Just(Pair->new("123", "abc")),
    "digits->parse(\"123abc\") should be equal to Just((\"123\", \"abc\"))");
  is(SomeParsers->digits->parse("123  abc"), Maybe->Just(Pair->new("123", "  abc")),
    "digits->parse(\"123  abc\") should be equal to Just((\"123\", \"  abc\"))");
  is(SomeParsers->digits->parse("abc"), Maybe->Just(Pair->new("", "abc")),
    "digits->parse(\"abc\") should be equal to Just((\"\", \"abc\"))");
}

sub test_double {
  is(SomeParsers->double->parse("1 abc"), Maybe->Just(Pair->new(1.0, "abc")),
    "double->parse(\"1 abc\") should be equal to Just((1.0, \"abc\"))");
  is(SomeParsers->double->parse("1. abc"), Maybe->Just(Pair->new(1.0, "abc")),
    "double->parse(\"1. abc\") should be equal to Just((1.0, \"abc\"))");
  is(SomeParsers->double->parse("1.23 abc"), Maybe->Just(Pair->new(1.23, "abc")),
    "double->parse(\"1.23 abc\") should be equal to Just((1.23, \"abc\"))");
  is(SomeParsers->double->parse("-1.23 abc"), Maybe->Nothing,
    "double->parse(\"-1.23 abc\") should be equal to Nothing");
  is(SomeParsers->double->parse(".23 abc"), Maybe->Just(Pair->new(0.23, "abc")),
    "double->parse(\".23 abc\") should be equal to Just((0.23, \"abc\"))");
  is(SomeParsers->double->parse(" + 1.23 abc"), Maybe->Nothing,
    "double->parse(\" + 1.23 abc\") should be equal to Nothing");
  is(SomeParsers->double->parse("1.23e10abc"), Maybe->Just(Pair->new(1.23e10, "abc")),
    "double->parse(\"1.23e10abc\") should be equal to Just((1.23e10, \"abc\"))");
  is(SomeParsers->double->parse("1.23e-10abc"), Maybe->Just(Pair->new(1.23e-10, "abc")),
    "double->parse(\"1.23e-10abc\") should be equal to Just((1.23e-10, \"abc\"))");
  is(SomeParsers->double->parse("abc"), Maybe->Nothing,
    "double->parse(\"abc\") should be equal to Nothing")
}

sub test_between {
  my $expr = Parser->between(SomeParsers->symbol('('), SomeParsers->symbol(')'), sub { SomeParsers->double });

  is($expr->parse(" ( 123 ) abc"), Maybe->Just(Pair->new(123.0, "abc")),
    "expr->parse(\" ( 123 ) abc\") should be equal to Just((123.0, \"abc\"))");
  is($expr->parse(" ( 123 abc"), Maybe->Nothing,
    "expr->parse(\" ( 123 abc\") should be equal to Nothing");
  is($expr->parse(" 123 ) abc"), Maybe->Nothing,
    "expr->parse(\" 123 ) abc\") should be equal to Nothing");
}

sub test_chainlr1 {
  my $add = SomeParsers->symbol('+') >> sub { Parser->pure(sub { shift() + shift }) };
  my $sub = SomeParsers->symbol('-') >> sub { Parser->pure(sub { shift() - shift }) };
  my $pow = SomeParsers->symbol('^') >> sub { Parser->pure(sub { exp(log(shift) * shift) }) };

  my $pexpr = SomeParsers->double->chainl1($add | sub { $sub }, 0);

  is($pexpr->parse("7abc"), Maybe->Just(Pair->new(7.0, "abc")),
    "expr->parse(\"7abc\") should be equal to Just((7.0, \"abc\"))");
  is($pexpr->parse(" 7 - 1 - 2 abc"), Maybe->Just(Pair->new(4.0, "abc")),
    "expr->parse(\" 7 - 1 - 2 abc\") should be equal to Just((4.0, \"abc\"))");
  is($pexpr->parse(" 7 - 1 + 2 - 3 abc"), Maybe->Just(Pair->new(5.0, "abc")),
    "expr->parse(\" 7 - 1 + 2 - 3 abc\") should be equal to Just((5.0, \"abc\"))");
  is($pexpr->parse("abc"), Maybe->Nothing, "expr->parse(\"abc\") should be equal to Nothing");
  is(SomeParsers->double->chainr1($pow)->parse("3 ^ 2 ^ 3 abc")->fmap(sub {
      my $pair = shift; Pair->new(int($pair->{first} + 0.5), $pair->{second});
    }), Maybe->Just(Pair->new(6561.0, "abc")),
    "expr->parse(\"3 ^ 2 ^ 3 abc\") should be equal to Just((6561.0, \"abc\"))");
}

test_char;
test_symbol;
test_alnum;
test_sign;
test_digits;
test_double;
test_between;
test_chainlr1;
done_testing;
