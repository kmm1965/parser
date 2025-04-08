#include <cmath>

#include <iostream>
#include <functional>
#include <numeric> // reduce

#include <boost/parser/parser.hpp>
namespace bp = boost::parser;

inline double sqr(double x){ return x * x; }

template<typename F>
auto _(F const& f){
    return std::function(f);
}

template<typename T>
auto _(T(*f)(T)){
    return std::function(f);
}

template<typename T>
using unary_function = std::function<T(T)>;

template<typename T>
using binary_function = std::function<T(T, T)>;

auto const add  = '+' >> bp::attr(_(std::plus<double>()));
auto const sub  = '-' >> bp::attr(_(std::minus<double>()));
auto const mul  = '*' >> bp::attr(_(std::multiplies<double>()));
auto const div_ = '/' >> bp::attr(_(std::divides<double>()));

#define FUNC(name) #name >> bp::attr(_<double>(::name))

auto const func =
    FUNC(sin)  | FUNC(cos)  | FUNC(asin)  | FUNC(acos)  |
    FUNC(sinh) | FUNC(cosh) | FUNC(asinh) | FUNC(acosh) |
    FUNC(tan)  | FUNC(log)  | FUNC(log10) | FUNC(exp)   |
    FUNC(sqrt) | FUNC(sqr);

#undef FUNC

#define CONST_(name) #name >> bp::attr(M_##name)

auto const const_ =
    CONST_(E)        | // 2.71828182845904523536   // e
    CONST_(LOG2E)    | // 1.44269504088896340736   // log2(e)
    CONST_(LOG10E)   | // 0.434294481903251827651  // log10(e)
    CONST_(LN2)      | // 0.693147180559945309417  // ln(2)
    CONST_(LN10)     | // 2.30258509299404568402   // ln(10)
    CONST_(PI)       | // 3.14159265358979323846   // pi
    CONST_(PI_2)     | // 1.57079632679489661923   // pi/2
    CONST_(PI_4)     | // 0.785398163397448309616  // pi/4
    CONST_(1_PI)     | // 0.318309886183790671538  // 1/pi
    CONST_(2_PI)     | // 0.636619772367581343076  // 2/pi
    CONST_(2_SQRTPI) | // 1.12837916709551257390   // 2/sqrt(pi)
    CONST_(SQRT2)    | // 1.41421356237309504880   // sqrt(2)
    CONST_(SQRT1_2);   // 0.707106781186547524401  // 1/sqrt(2)

#undef CONST_

auto const chainl1 = [](auto const& tp){
    if constexpr(std::tuple_size_v<std::decay_t<decltype(tp)> > == 3)
        return std::reduce(std::cbegin(std::get<2>(tp)), std::cend(std::get<2>(tp)), std::get<0>(tp) == '-' ? -std::get<1>(tp) : std::get<1>(tp),
            [](double val, auto const& op){ return std::get<0>(op)(val, std::get<1>(op)); });
    else return std::reduce(std::cbegin(std::get<1>(tp)), std::cend(std::get<1>(tp)), std::get<0>(tp),
            [](double val, auto const& op){ return std::get<0>(op)(val, std::get<1>(op)); });
};

#define DEFINE_RULE(name) bp::rule<struct name##_tag, double> name = #name

DEFINE_RULE(expr);
DEFINE_RULE(term);
DEFINE_RULE(factor);
DEFINE_RULE(factor0);
DEFINE_RULE(expr_in_brackets);

#undef DEFINE_RULE

auto const expr_def = bp::transform(chainl1)[-(bp::char_('+') | bp::char_('-')) >> term >> *((add | sub) >> term)];
auto const term_def   = bp::transform(chainl1)[factor >> *((mul | div_) >> factor)];
auto const factor_def = bp::transform([](auto const& vec)
    { return std::reduce(std::crbegin(vec) + 1, std::crend(vec), vec.back(), [](double y, double x) { return std::pow(x, y); }); })
    [factor0 % '^'];
auto const factor0_def =
    expr_in_brackets |
    bp::transform([](auto const& tp){ return std::get<0>(tp)(std::get<1>(tp)); })[func >> expr_in_brackets] |
    const_ |
    bp::double_;
auto const expr_in_brackets_def = '(' >> expr > ')';

BOOST_PARSER_DEFINE_RULES(expr, term, factor, factor0, expr_in_brackets);

int main()
{
    std::cout
        << bp::parse(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)", expr, bp::ws).value() << std::endl
        << bp::parse(" sin(2_SQRTPI * sqr(2) - 1)", expr, bp::ws).value() << std::endl
        << bp::parse(" sqrt(exp(E * sin(2.2 * 2_PI)))", expr, bp::ws).value() << std::endl
        << bp::parse("sqr(2_PI)", expr, bp::ws).value() << std::endl
        << bp::parse("sqr( sin (2)) + sqr(cos(1 + 1))", expr, bp::ws).value() << std::endl
        << bp::parse(" 3 ^ (1 + 1) ^ 3", expr, bp::ws).value() << std::endl
        << bp::parse(" E ^ PI", expr, bp::ws).value() << std::endl
        << bp::parse(" sin(-PI/4) ", expr, bp::ws).value() << std::endl
        << bp::parse(" sin(+PI/4) ", expr, bp::ws).value() << std::endl
        << bp::parse("- 2 ^ 2", expr, bp::ws).value() << std::endl
        ;
}
