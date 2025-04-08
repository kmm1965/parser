#include <cmath>

#include <iostream>
#include <string>
#include <functional>
#include <numeric> // reduce

#include <boost/spirit/include/qi.hpp>
#include <boost/phoenix/operator.hpp>
namespace qi = boost::spirit::qi;

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

struct func_call_expression;
struct add_expression;
struct mul_expression;
struct expr_term;
struct pow_expression;

struct calc_expr
{
    calc_expr() : value(0){}
    calc_expr(double value) : value(value){}
    calc_expr(func_call_expression const& fcall);
    calc_expr(add_expression const& bexpr);
    calc_expr(mul_expression const& bexpr);
    calc_expr(pow_expression const& pexpr);

    double value;
};

struct func_call_expression
{
    unary_function<double> func;
    calc_expr expr;
};

BOOST_FUSION_ADAPT_STRUCT(
	func_call_expression,
	func,
	expr
)

struct expr_term
{
	binary_function<double> op;
	calc_expr term;
};

BOOST_FUSION_ADAPT_STRUCT(
	expr_term,
	op,
	term
)

struct add_expression
{
    char negate_first;
	calc_expr term;
	std::vector<expr_term> terms;
};

BOOST_FUSION_ADAPT_STRUCT(
	add_expression,
    negate_first,
	term,
	terms
)

struct mul_expression
{
	calc_expr term;
	std::vector<expr_term> terms;
};

BOOST_FUSION_ADAPT_STRUCT(
    mul_expression,
	term,
	terms
)

struct pow_expression
{
    calc_expr term;
    std::vector<calc_expr> terms;
};

BOOST_FUSION_ADAPT_STRUCT(
    pow_expression,
    term,
    terms
)

calc_expr::calc_expr(func_call_expression const& fcall) : value(fcall.func(fcall.expr.value)){}

calc_expr::calc_expr(add_expression const& aexpr) :
    value(std::reduce(aexpr.terms.cbegin(), aexpr.terms.cend(), aexpr.negate_first == '-' ? -aexpr.term.value : aexpr.term.value,
        [](double val, expr_term const& term){ return term.op(val, term.term.value); })){}

calc_expr::calc_expr(mul_expression const& mexpr) :
    value(std::reduce(mexpr.terms.cbegin(), mexpr.terms.cend(), mexpr.term.value,
        [](double val, expr_term const& term){ return term.op(val, term.term.value); })){}

calc_expr::calc_expr(pow_expression const& pexpr) :
    value(pexpr.terms.empty() ? pexpr.term.value :
        std::pow(pexpr.term.value, std::reduce(pexpr.terms.crbegin() + 1, pexpr.terms.crend(), pexpr.terms.back().value,
            [](double y, calc_expr const& e){ return std::pow(e.value, y); }))){}

template <typename Iterator>
struct calculator_grammar : qi::grammar<Iterator, calc_expr(), qi::space_type>
{
    calculator_grammar() : calculator_grammar::base_type(expr)
    {
        using qi::char_;
        using qi::lit;
        using qi::_val;

        add  = lit('+')[_val = _(std::plus<double>())];
        sub  = lit('-')[_val = _(std::minus<double>())];
        mul  = lit('*')[_val = _(std::multiplies<double>())];
        div_ = lit('/')[_val = _(std::divides<double>())];

        addsub %= (add | sub) >> term;
        muldiv %= (mul | div_) >> factor;
        pow0 %= '^' >> factor0;

        add_expr %= -(char_('+') | char_('-')) >> term >> *addsub;
        mul_expr %= factor >> *muldiv;
        pow_expr %= factor0 >> *pow0;

#define FUNC(name) lit(#name)[_val = _<double>(::name)]

        func =
            FUNC(sin)  | FUNC(cos)  | FUNC(asin)  | FUNC(acos)  |
            FUNC(sinh) | FUNC(cosh) | FUNC(asinh) | FUNC(acosh) |
            FUNC(tan)  | FUNC(log)  | FUNC(log10) | FUNC(exp)   |
            FUNC(sqrt) | FUNC(sqr);

        #undef FUNC

        func_call %= func >> expr_in_brackets;

        #define CONST_(name) lit(#name)[_val = M_##name]

        const_ =
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

        expr   %= add_expr;
        term   %= mul_expr;
        factor %= pow_expr;

        factor0 %=
            expr_in_brackets |
            func_call |
            const_ |
            qi::double_;

        expr_in_brackets %= '(' >> expr > ')';
    }

    qi::rule<Iterator, binary_function<double>(), qi::space_type> add, sub, mul, div_;
    qi::rule<Iterator, expr_term(), qi::space_type> addsub, muldiv;
    qi::rule<Iterator, add_expression(), qi::space_type> add_expr;
    qi::rule<Iterator, mul_expression(), qi::space_type> mul_expr;
    qi::rule<Iterator, pow_expression(), qi::space_type> pow_expr;
    qi::rule<Iterator, unary_function<double>(), qi::space_type> func;
    qi::rule<Iterator, func_call_expression(), qi::space_type> func_call;
    qi::rule<Iterator, double(), qi::space_type> const_;
    qi::rule<Iterator, calc_expr(), qi::space_type> expr, term, factor, pow0, factor0, expr_in_brackets;
};

void calc(std::string const& strm)
{
    using Iterator = std::string::const_iterator;
    Iterator first = strm.cbegin();
    Iterator const last = strm.cend();
    calculator_grammar<Iterator> calc; // Our grammar

    calc_expr expr; // Our tree
    bool r = phrase_parse(first, last, calc, qi::space, expr);
    if(r && first == last)
        std::cout << expr.value << std::endl;
    else std::cout
        << "-------------------------" << std::endl
        << "Parsing failed" << std::endl
        << "stopped at: \": " << std::string(first, last) << "\"" << std::endl << std::endl
        << "-------------------------" << std::endl;
}

int main()
{
    calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)");  // -8.889
    calc(" sin(2_SQRTPI * sqr(2) - 1)");            // - 0.363409
    calc(" sqrt(exp(E * sin(2.2 * 2_PI)))");        // 3.81712
    calc("sqr(2_PI)");                              // 0.405285
    calc("sqr( sin (2)) + sqr(cos(1 + 1))");        // 1
    calc(" 3 ^ (1 + 1) ^ 3");                       // 6561
    calc(" E ^ PI");                                // 23.1407
    calc(" PI ^ E");                                // 22.4592
    calc(" sin(-PI/4) ");                           // -0.707107
    calc(" sin(+PI/4) ");                           // 0.707107
    calc("- 2 ^ 2");                                // -4
}
