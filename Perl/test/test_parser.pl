require "./src/parser.pl";

use strict;
use warnings;
use Test::More; #tests => 2; # Declare the number of expected tests

sub test_parser_pure {
  is(Parser->pure(1)->parse("abc"), Maybe->Just(Pair->new(1, "abc")),
    "pure(1)->parse(\"abc\") should be equal to Just((1, \"abc\"))");
  is(Parser->pure(1.0)->parse("abc"), Maybe->Just(Pair->new(1.0, "abc")),
    "pure(1)->parse(\"abc\") should be equal to Just((1.0, \"abc\"))");
  is(Parser->pure("1")->parse("abc"), Maybe->Just(Pair->new("1", "abc")),
    "pure(1)->parse(\"abc\") should be equal to Just((\"1\", \"abc\"))");
}

sub test_parser_map {
  my $toString = sub { my $x = shift; "$x" };
  my $toInt = sub { int(shift) };

  is(Parser->pure(1)->fmap($toString)->parse("abc"), Maybe->Just(Pair->new("1", "abc")),
    "pure(1)->fmap(toString) should be equal to Just((\"1\", \"abc\"))");
  is(Parser->pure(1.0)->fmap($toString)->parse("abc"), Maybe->Just(Pair->new("1", "abc")),
    "pure(1.0)->fmap(toString) should be equal to Just((\"1\", \"abc\"))");
  is(Parser->pure("1")->fmap($toInt)->parse("abc"), Maybe->Just(Pair->new(1, "abc")),
    "pure(\"1\")->fmap(toInt) should be equal to Just((1, \"abc\"))");

  is(Parser->empty->fmap($toString)->parse("abc"), Maybe->Nothing,
    "empty->fmap(toString) should be equal to Nothing");
  is(Parser->empty->fmap($toInt)->parse("abc"), Maybe->Nothing,
    "empty->fmap(toInt) should be equal to Nothing");
}

sub test_parser_andThen {
  my $eat = sub {
    my $i = shift;
    Parser->new(sub {
      Maybe->Just(Pair->new("$i" . shift, ""));
    });
  };

  my $cancel = sub { Parser->empty };
  my $i1 = Parser->pure(1);

  is($i1->andThen($eat)->parse("abc"), Maybe->Just(Pair->new("1abc", "")),
    "pure(1)->andThen(eat)->parse(\"abc\") should be equal to Just((\"1abc\", \"\"))");
  is($i1->andThen($cancel)->parse("abc"), Maybe->Nothing,
    "pure(1)->andThen(cancel)->parse(\"abc\") should be equal to Nothing");
  is(Parser->empty->andThen($eat)->parse("abc"), Maybe->Nothing,
    "empty->andThen(eat)->parse(\"abc\") should be equal to Nothing");
  is(Parser->empty->andThen($cancel)->parse("abc"), Maybe->Nothing,
    "empty->andThen(cancel)->parse(\"abc\") should be equal to Nothing");
}

sub test_apply {
  my $fsin = sub { sin(shift) };
  my $psin = Parser->pure($fsin);
  my $f1 = sub { Parser->pure(1.0) };
  my $fe = sub { Parser->empty };

  is($psin->apply($f1)->parse("abc"), Maybe->Just(Pair->new(sin(1.0), "abc")),
    "pure(sin)->apply(pure(1.0))->parse(\"abc\") should be equal to Just((sin(1.0), \"abc\")");
  is($psin->apply($fe)->parse("abc"), Maybe->Nothing,
    "pure(sin)->apply(empty)->parse(\"abc\") should be equal to Nothing");
  is(Parser->empty->apply($f1)->parse("abc"), Maybe->Nothing,
    "empty->apply(pure(1.0))->parse(\"abc\") should be equal to Nothing");
  is(Parser->empty->apply($fe)->parse("abc"), Maybe->Nothing,
    "empty->apply(empty)->parse(\"abc\") should be equal to Nothing");
}

sub test_empty_string {
  is(Parser->empty_string->parse("abc"), Maybe->Just(Pair->new("", "abc")),
    "empty_string->parse(\"abc\") should be equal to Just((\"\", \"abc\")");
}

sub test_any_char {
  is(Parser->any_char->parse("abc"), Maybe->Just(Pair->new('a', "bc")),
    "any_char->parse(\"abc\") should be equal to Just(('a', \"bc\")");
  is(Parser->any_char->parse(""), Maybe->Nothing,
    "any_char->parse(\"\") should be equal to Nothing");
}

sub test_satisfy {
  is(Parser->satisfy(sub { shift eq 'a' })->parse("abc"), Maybe->Just(Pair->new('a', "bc")),
    "satisfy(c == 'a')->parse(\"abc\") should be equal to Just(('a', \"bc\")");
  is(Parser->satisfy(sub { shift eq 'z' })->parse("abc"), Maybe->Nothing,
    "satisfy(c == 'z')->parse(\"abc\") should be equal to Nothing");
  is(Parser->satisfy(sub{ shift eq 'a' })->parse(""), Maybe->Nothing,
    "satisfy(c == 'a')->parse(\"\") should be equal to Nothing");
}

sub test_spaces {
  is(Parser->spaces->parse("abc"), Maybe->Just(Pair->new("", "abc")),
    "spaces->parse(\"abc\") should be equal to Just((\"\", \"abc\"))");
  is(Parser->spaces->parse("   abc"), Maybe->Just(Pair->new("   ", "abc")),
    "spaces->parse(\"   abc\") should be equal to Just((\"   \", \"abc\"))");
}

test_parser_pure;
test_parser_map;
test_parser_andThen;
test_apply;
test_empty_string;
test_any_char;
test_satisfy;
test_spaces;
done_testing;
