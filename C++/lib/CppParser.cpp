#include "Calculator.hpp"

#include <cassert>

#include <iostream>

int main()
{
    Parser<double> expr = Calculator().expr();

    auto const pair2double = [](parser_pair<double> const& p)
    {
        assert(p.second.empty());
        return p.first;
    };
    std::cout
        << expr.parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").transform(pair2double).value() << std::endl
        << expr.parse("-2*2").transform(pair2double).value() << std::endl
        << expr.parse("+2^2").transform(pair2double).value() << std::endl
        << expr.parse(" sin(2_SQRTPI * sqr(2) - 1)").transform(pair2double).value() << std::endl
        << expr.parse(" sqrt(exp(E * sin(2.2 * 2_PI)))").transform(pair2double).value() << std::endl
        << expr.parse("sqr(2_PI)").transform(pair2double).value() << std::endl
        << expr.parse("sqr( sin (2)) + sqr(cos(1 + 1))").transform(pair2double).value() << std::endl
        << expr.parse("3^2^3").transform(pair2double).value() << std::endl
        << expr.parse(" E^PI").transform(pair2double).value() << std::endl;
        
}
