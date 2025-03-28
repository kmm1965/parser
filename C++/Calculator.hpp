#pragma once

#include "SomeParsers.hpp"

inline double sqr(double x){ return x * x; }

class Calculator
{
private:
    using bin_func_t = binary_function<double>;
    using Parser_f = Parser<bin_func_t>;
    using Parser_d = Parser<double>;
    using Parser_c = Parser<char>;
    using func_t = unary_function<double>;
    using Parser_func = Parser<func_t>;

    static Parser_f op2(char c, bin_func_t const& f){
        return symbol(c) >> Parser_f::pure(f);
    }

    Parser_f const
        add = op2('+', std::plus<double>()),
        sub = op2('-', std::minus<double>()),
        mul = op2('*', std::multiplies<double>()),
        div = op2('/', std::divides<double>()),
        pow = op2('^', [](double x, double y){ return std::pow(x, y); });

    template<typename A>
    static Parser<A> def_object(const char* name, A const& value){
        return _name(name) >> Parser<A>::pure(value);
    }

    #define FUNC(name) def_object(#name, _<double>(::name))

    Parser_func const func =
        FUNC(sin)  | FUNC(cos)  | FUNC(asin)  | FUNC(acos)  |
        FUNC(sinh) | FUNC(cosh) | FUNC(asinh) | FUNC(acosh) |
        FUNC(tan)  | FUNC(log)  | FUNC(log10) | FUNC(exp)   |
        FUNC(sqrt) | FUNC(sqr);

    #undef FUNC

    #define CONST(name) def_object(#name, M_##name)

    //std::array<Parser_d, 13> const constants {
    Parser_d const _const =
        CONST(E)        | // 2.71828182845904523536   // e
        CONST(LOG2E)    | // 1.44269504088896340736   // log2(e)
        CONST(LOG10E)   | // 0.434294481903251827651  // log10(e)
        CONST(LN2)      | // 0.693147180559945309417  // ln(2)
        CONST(LN10)     | // 2.30258509299404568402   // ln(10)
        CONST(PI)       | // 3.14159265358979323846   // pi
        CONST(PI_2)     | // 1.57079632679489661923   // pi/2
        CONST(PI_4)     | // 0.785398163397448309616  // pi/4
        CONST(1_PI)     | // 0.318309886183790671538  // 1/pi
        CONST(2_PI)     | // 0.636619772367581343076  // 2/pi
        CONST(2_SQRTPI) | // 1.12837916709551257390   // 2/sqrt(pi)
        CONST(SQRT2)    | // 1.41421356237309504880   // sqrt(2)
        CONST(SQRT1_2);   // 0.707106781186547524401  // 1/sqrt(2)

    #undef CONST

    Parser_d expr_in_brackets() const {
        return between(symbol('('), symbol(')'), _([self = *this](){ return self.expr(); }));
    }

    Parser_d factor0() const {
        return expr_in_brackets()
            // make expr_in_brackets lasy
            | func * _([self = *this](){ return self.expr_in_brackets(); })
            | _const
            | _double;
    }

    Parser_d factor() const {
        return factor0().chainr1(pow);
    }

    Parser_d term() const {
        return factor().chainl1(mul | div);
    }

public:
    Parser_d expr() const {
        return term().chainl1(add | sub);
    }
};
