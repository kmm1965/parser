#pragma once

#include "../lib/SomeParsers.hpp"

BOOST_AUTO_TEST_CASE(test_alnum)
{
    BOOST_TEST(alnum.parse("123abc  ") == Just(std::make_pair('1', "23abc  "s)));
    BOOST_TEST(alnum.parse("_123  abc") == Just(std::make_pair('_', "123  abc"s)));
    BOOST_TEST(alnum.parse("!@%$") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_char)
{
    BOOST_TEST(char_('a').parse("abc") == Just(std::make_pair('a', "bc"s)));
    BOOST_TEST(char_('z').parse("abc") == Nothing);

    BOOST_TEST('a'_l.parse("abc") == Just(std::make_pair('a', "bc"s)));
    BOOST_TEST('z'_l.parse("abc") == Nothing);

    BOOST_TEST(('a'_l >> 'b'_l).parse("abc") == Just(std::make_pair('b', "c"s)));
}

BOOST_AUTO_TEST_CASE(test_symbol)
{
    BOOST_TEST('+'_s.parse(" + abc") == Just(std::make_pair('+', "abc"s)));
    BOOST_TEST('+'_s.parse("abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_name)
{
    Parser<std::string> const nsin = "sin"_n;
        
    BOOST_TEST(nsin.parse(" sin ") == Just(std::make_pair("sin", ""s)));
    BOOST_TEST(nsin.parse("  sin  (1.)") == Just(std::make_pair("sin", "(1.)"s)));
    BOOST_TEST(nsin.parse("sinabc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_optional_a)
{
    BOOST_TEST(optional_l<char>("sin"_n).parse(" sin abc") == Just(std::make_pair("sin", "abc"s)));
    BOOST_TEST(optional_l<char>("sin"_n).parse("abc") == Just(std::make_pair("", "abc"s)));

    BOOST_TEST((-double_).parse("abc") == Just(std::make_pair(std::list<double>(), "abc"s)));
    BOOST_TEST((-double_).parse("123abc") == Just(std::make_pair(std::list<double>{ 123 }, "abc"s)));

    BOOST_TEST((double_.many()).parse("abc") == Just(std::make_pair(std::list<double>(), "abc"s)));
    BOOST_TEST((double_.many()).parse("123 456 abc") == Just(std::make_pair(std::list<double>{ 123, 456 }, "abc"s)));
}

BOOST_AUTO_TEST_CASE(test_double)
{
    BOOST_TEST(double_.parse(" 1 abc") == Just(std::make_pair(1, "abc"s)));
    BOOST_TEST(double_.parse(" 1. abc") == Just(std::make_pair(1, "abc"s)));
    BOOST_TEST(double_.parse(" 1.23 abc") == Just(std::make_pair(1.23, "abc"s)));
    BOOST_TEST(double_.parse(" .23 abc") == Just(std::make_pair(0.23, "abc"s)));
    BOOST_TEST(double_.parse(" +1.23 abc") == Just(std::make_pair(+1.23, "abc"s)));
    BOOST_TEST(double_.parse(" -1.23 abc") == Just(std::make_pair(-1.23, "abc"s)));
    BOOST_TEST(double_.parse("1.23e10abc") == Just(std::make_pair(1.23e10, "abc"s)));
    BOOST_TEST(double_.parse("1.23e-10abc") == Just(std::make_pair(1.23e-10, "abc"s)));
    BOOST_TEST(double_.parse("-1.23e-10abc") == Just(std::make_pair(-1.23e-10, "abc"s)));
    BOOST_TEST(double_.parse("abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_between)
{
    Parser<double> const expr = between('('_s, ')'_s, _([](){ return double_; }));

    BOOST_TEST(expr.parse(" ( 123 ) abc") == Just(std::make_pair(123, "abc"s)));
    BOOST_TEST(expr.parse(" ( 123 abc") == Nothing);
    BOOST_TEST(expr.parse(" 123 ) abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_chainlr1)
{
    #define FUNC(body) _([](){ return Parser<binary_function<double> >::pure(_([](double x, double y){ return body; })); })

    Parser<binary_function<double> > const
        add = '+'_s >> FUNC(x + y),
        sub = '-'_s >> FUNC(x - y),
        pow = '^'_s >> FUNC(std::pow(x, y));

    #undef FUNC

    Parser<double> const expr = double_.chainl1(add | sub);

    BOOST_TEST(expr.parse("7") == Just(std::make_pair(7, ""s)));
    BOOST_TEST(expr.parse("7abc") == Just(std::make_pair(7, "abc"s)));
    BOOST_TEST(expr.parse("7-1") == Just(std::make_pair(6, ""s)));
    BOOST_TEST(expr.parse(" 7 - 1 - 2 abc") == Just(std::make_pair(4, "abc"s)));
    BOOST_TEST(expr.parse(" 7 - 1 + 2 - 3 abc") == Just(std::make_pair(5, "abc"s)));
    BOOST_TEST(expr.parse("abc") == Nothing);

    BOOST_TEST(double_.chainr1(pow).parse("3 ^ 2 ^ 3 abc") == Just(std::make_pair(6561, "abc"s)));
}
