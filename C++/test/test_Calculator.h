#pragma once

#include "../lib/Calculator.hpp"

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
