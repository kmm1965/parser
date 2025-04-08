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

    #define OP20(op, func) op##_s >> Parser_f::pure(func)
    #define OP2(op, name) OP20(op, std::name<double>())

    Parser_f const
        add = OP2('+', plus),
        sub = OP2('-', minus),
        mul = OP2('*', multiplies),
        div = OP2('/', divides),
        pow = OP20('^', [](double x, double y){ return std::pow(x, y); });

    #undef OP2
    #undef OP20

    #define FUNC(name) #name##_n >> Parser<func_t>::pure(_<double>(::name))

    Parser_func const func =
        FUNC(sin)  | FUNC(cos)  | FUNC(asin)  | FUNC(acos)  |
        FUNC(sinh) | FUNC(cosh) | FUNC(asinh) | FUNC(acosh) |
        FUNC(tan)  | FUNC(log)  | FUNC(log10) | FUNC(exp)   |
        FUNC(sqrt) | FUNC(sqr);

    #undef FUNC

    #define CONST(name) #name##_n >> Parser<double>::pure(M_##name)

    Parser_d const const_ =
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
        return between('('_s, ')'_s, _([self = *this](){ return self.expr(); }));
    }

    Parser_d factor0() const {
        return expr_in_brackets()
            | func * _([self = *this](){ return self.expr_in_brackets(); })
            | const_
            | double_;
    }

    Parser_d factor() const {
        return factor0().chainr1(pow);
    }

    Parser_d term() const {
        return factor().chainl1(mul | div);
    }

public:
    Parser_d expr() const {
        Calculator const self = *this;
        return _do(sgn, usign, self.term().chainl1(self.add | self.sub, sgn == "-"));
    }
};
