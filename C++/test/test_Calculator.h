#pragma once

#include "../lib/Calculator.hpp"

BOOST_AUTO_TEST_CASE(test_funcs)
{
    Parser<double> expr = Calculator().expr();

    BOOST_TEST(expr.parse("sin(2.0)") == Just(std::make_pair(sin(2.0), ""s)));
    BOOST_TEST(expr.parse("cos(2.0)") == Just(std::make_pair(cos(2.0), ""s)));
    BOOST_TEST(expr.parse("asin(0.5)") == Just(std::make_pair(asin(0.5), ""s)));
    BOOST_TEST(expr.parse("acos(0.5)") == Just(std::make_pair(acos(0.5), ""s)));
    BOOST_TEST(expr.parse("sinh(2.0)") == Just(std::make_pair(sinh(2.0), ""s)));
    BOOST_TEST(expr.parse("cosh(2.0)") == Just(std::make_pair(cosh(2.0), ""s)));
    BOOST_TEST(expr.parse("asinh(2.0)") == Just(std::make_pair(asinh(2.0), ""s)));
    BOOST_TEST(expr.parse("acosh(2.0)") == Just(std::make_pair(acosh(2.0), ""s)));
    BOOST_TEST(expr.parse("tan(2.0)") == Just(std::make_pair(tan(2.0), ""s)));
    BOOST_TEST(expr.parse("log(2.0)") == Just(std::make_pair(log(2.0), ""s)));
    BOOST_TEST(expr.parse("log10(2.0)") == Just(std::make_pair(log10(2.0), ""s)));
    BOOST_TEST(expr.parse("exp(2.0)") == Just(std::make_pair(exp(2.0), ""s)));
    BOOST_TEST(expr.parse("sqrt(2.0)") == Just(std::make_pair(sqrt(2.0), ""s)));
    BOOST_TEST(expr.parse("sqr(2.0)") == Just(std::make_pair(4.0, ""s)));
}

BOOST_AUTO_TEST_CASE(test_consts)
{
    Parser<double> expr = Calculator().expr();

    BOOST_TEST(expr.parse("E") == Just(std::make_pair(M_E, ""s)));
    BOOST_TEST(expr.parse("LOG2E") == Just(std::make_pair(M_LOG2E, ""s)));
    BOOST_TEST(expr.parse("LOG2E") == Just(std::make_pair(1 / M_LN2, ""s)));
    BOOST_TEST(expr.parse("LOG10E") == Just(std::make_pair(M_LOG10E, ""s)));
    //BOOST_TEST(expr.parse("LOG10E") == Just(std::make_pair(1 / log(10), ""s)));
    BOOST_TEST(expr.parse("LN2") == Just(std::make_pair(M_LN2, ""s)));
    BOOST_TEST(expr.parse("LN2") == Just(std::make_pair(log(2), ""s)));
    BOOST_TEST(expr.parse("LN10") == Just(std::make_pair(M_LN10, ""s)));
    BOOST_TEST(expr.parse("LN10") == Just(std::make_pair(log(10), ""s)));
    BOOST_TEST(expr.parse("PI") == Just(std::make_pair(M_PI, ""s)));
    BOOST_TEST(expr.parse("PI_2") == Just(std::make_pair(M_PI_2, ""s)));
    BOOST_TEST(expr.parse("PI_2") == Just(std::make_pair(M_PI / 2, ""s)));
    BOOST_TEST(expr.parse("PI_4") == Just(std::make_pair(M_PI_4, ""s)));
    BOOST_TEST(expr.parse("PI_4") == Just(std::make_pair(M_PI / 4, ""s)));
    BOOST_TEST(expr.parse("1_PI") == Just(std::make_pair(M_1_PI, ""s)));
    BOOST_TEST(expr.parse("1_PI") == Just(std::make_pair(1 / M_PI, ""s)));
    BOOST_TEST(expr.parse("2_PI") == Just(std::make_pair(M_2_PI, ""s)));
    BOOST_TEST(expr.parse("2_PI") == Just(std::make_pair(2 / M_PI, ""s)));
    BOOST_TEST(expr.parse("2_SQRTPI") == Just(std::make_pair(M_2_SQRTPI, ""s)));
    BOOST_TEST(expr.parse("2_SQRTPI") == Just(std::make_pair(2 / sqrt(M_PI), ""s)));
    BOOST_TEST(expr.parse("SQRT2") == Just(std::make_pair(M_SQRT2, ""s)));
    BOOST_TEST(expr.parse("SQRT2") == Just(std::make_pair(sqrt(2), ""s)));
    BOOST_TEST(expr.parse("SQRT1_2") == Just(std::make_pair(M_SQRT1_2, ""s)));
    BOOST_TEST(expr.parse("SQRT1_2") == Just(std::make_pair(sqrt(0.5), ""s)));
}

BOOST_AUTO_TEST_CASE(test_unary)
{
    Parser<double> expr = Calculator().expr();

    BOOST_TEST(expr.parse("-PI") == Just(std::make_pair(-M_PI, ""s)));
    BOOST_TEST(expr.parse("-2^2") == Just(std::make_pair(-4., ""s)));
    BOOST_TEST(expr.parse("5+-3") == Just(std::make_pair(5., "+-3"s)));
}

BOOST_AUTO_TEST_CASE(test_Calculator)
{
    Parser<double> expr = Calculator().expr();

    BOOST_TEST(expr.parse("72 - 7 - (1 - 2) * 3") == Just(std::make_pair(68, ""s)));
    BOOST_TEST(expr.parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)") == Just(std::make_pair(-8.889, ""s)));
    BOOST_TEST(expr.parse("3^(1+1)^3") == Just(std::make_pair(6561, ""s)));
    BOOST_TEST(expr.parse("sin(1+1)") == Just(std::make_pair(std::sin(2), ""s)));
    BOOST_TEST(expr.parse("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )") == Just(std::make_pair(-0.3634085731426532, ""s)));
    BOOST_TEST(expr.parse("sqr(2 + 3)") == Just(std::make_pair(25, ""s)));
    BOOST_TEST(expr.parse("E ^ PI") == Just(std::make_pair(std::pow(M_E, M_PI), ""s)));
    BOOST_TEST(expr.parse("PI ^ E") == Just(std::make_pair(std::pow(M_PI, M_E), ""s)));
}
