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
    BOOST_TEST(_char('a').parse("abc") == Just(std::make_pair('a', "bc"s)));
    BOOST_TEST(_char('z').parse("abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_symbol)
{
    BOOST_TEST(symbol('+').parse(" + abc") == Just(std::make_pair('+', "abc"s)));
    BOOST_TEST(symbol('+').parse("abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_name)
{
    Parser<std::string> const nsin = _name("sin");
        
    BOOST_TEST(nsin.parse(" sin ") == Just(std::make_pair("sin", ""s)));
    BOOST_TEST(nsin.parse("  sin  (1.)") == Just(std::make_pair("sin", "(1.)"s)));
    BOOST_TEST(nsin.parse("sinabc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_optional_s)
{
    BOOST_TEST(optional_s(_name("sin")).parse(" sin abc") == Just(std::make_pair("sin", "abc"s)));
    BOOST_TEST(optional_s(_name("sin")).parse("abc") == Just(std::make_pair("", "abc"s)));
}

BOOST_AUTO_TEST_CASE(test_double)
{
    BOOST_TEST(_double.parse(" 1 abc") == Just(std::make_pair(1, "abc"s)));
    BOOST_TEST(_double.parse(" 1. abc") == Just(std::make_pair(1, "abc"s)));
    BOOST_TEST(_double.parse(" 1.23 abc") == Just(std::make_pair(1.23, "abc"s)));
    BOOST_TEST(_double.parse(" .23 abc") == Just(std::make_pair(0.23, "abc"s)));
    BOOST_TEST(_double.parse(" +1.23 abc") == Just(std::make_pair(+1.23, "abc"s)));
    BOOST_TEST(_double.parse(" -1.23 abc") == Just(std::make_pair(-1.23, "abc"s)));
    BOOST_TEST(_double.parse("1.23e10abc") == Just(std::make_pair(1.23e10, "abc"s)));
    BOOST_TEST(_double.parse("1.23e-10abc") == Just(std::make_pair(1.23e-10, "abc"s)));
    BOOST_TEST(_double.parse("-1.23e-10abc") == Just(std::make_pair(-1.23e-10, "abc"s)));
    BOOST_TEST(_double.parse("abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_between)
{
    Parser<double> const expr = between(symbol('('), symbol(')'), _([](){ return _double; }));

    BOOST_TEST(expr.parse(" ( 123 ) abc") == Just(std::make_pair(123, "abc"s)));
    BOOST_TEST(expr.parse(" ( 123 abc") == Nothing);
    BOOST_TEST(expr.parse(" 123 ) abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_chainlr1)
{
    Parser<binary_function<double> > const
        add = symbol('+') >> _([]() { return Parser<binary_function<double> >::pure(_([](double x, double y){ return x + y; })); }),
        sub = symbol('-') >> _([]() { return Parser<binary_function<double> >::pure(_([](double x, double y){ return x - y; })); }),
        pow = symbol('^') >> _([]() { return Parser<binary_function<double> >::pure(_([](double x, double y){ return std::pow(x, y); })); });

    Parser<double> const expr = _double.chainl1(add | sub);

    BOOST_TEST(expr.parse("7") == Just(std::make_pair(7, ""s)));
    BOOST_TEST(expr.parse("7abc") == Just(std::make_pair(7, "abc"s)));
    BOOST_TEST(expr.parse("7-1") == Just(std::make_pair(6, ""s)));
    BOOST_TEST(expr.parse(" 7 - 1 - 2 abc") == Just(std::make_pair(4, "abc"s)));
    BOOST_TEST(expr.parse(" 7 - 1 + 2 - 3 abc") == Just(std::make_pair(5, "abc"s)));
    BOOST_TEST(expr.parse("abc") == Nothing);

    BOOST_TEST(_double.chainr1(pow).parse("3 ^ 2 ^ 3 abc") == Just(std::make_pair(6561, "abc"s)));
}
