#define _USE_MATH_DEFINES
#include <cmath>
#include <cassert>

#include <iostream>
#include <functional>
#include <string>
#include <optional>
#include <utility> // pair
#include <concepts>
#include <array>
#include <numeric>

double sqr(double x){ return x * x; }

template<typename F>
auto _(F const& f){
    return std::function(f);
}

template<typename T>
auto _(T(*f)(T)){
    return std::function(f);
}

template<typename T>
using supplier = std::function<T()>;

template<typename T>
using unary_function = std::function<T(T)>;

template<typename T>
using binary_function = std::function<T(T, T)>;

template<typename T>
using predicate = std::function<bool(T)>;

template<typename T>
using parser_pair = std::pair<T, std::string>;

template<typename T>
using parser_data = std::optional<parser_pair<T> >;

template<typename A>
struct Parser {
    using value_type = A;
    using pair_t = parser_pair<A>;
    using result_t = parser_data<A>;
    using function_t = std::function<result_t(std::string const&)>;
    using bin_func_t = binary_function<A>;

    Parser(function_t const& p) : p(p){}

    result_t parse(std::string const& inp) const {
        return p(inp);
    }

    /*
    instance Functor Parser where
        fmap :: (a -> b) -> Parser a -> Parser b
        fmap f p = P (\inp -> (\(x, out) -> (f x, out)) <$> parse p inp)
    */
    template<typename B>
    Parser<B> transform(std::function<B(A const&)> const& f) const
    {
        Parser const self(*this);
        return Parser<B>([self, f](std::string const& inp){
            return self.parse(inp).transform([f](pair_t const& p){
                return std::make_pair(f(p.first), p.second);
            });
        });
    }

    /*
    instance Applicative Parser where
        pure :: a -> Parser a
        pure f = P ( \inp -> Just (f, inp))
    */
    static Parser pure(A const& a){
        return Parser([a](std::string const& inp){
            return result_t(pair_t(a, inp));
        });
    }

    /*
    instance Monad Parser where
        (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= f = P (\inp -> parse p inp >>= (\(x, out) -> parse (f x) out))
    */
    template<typename B>
    Parser<B> and_then(std::function<Parser<B>(A const&)> const& f) const
    {
        Parser const self(*this);
        return Parser<B>([self, f](std::string const& inp){
            return self.parse(inp).and_then([f](pair_t const& p){
                return f(p.first).parse(p.second);
            });
        });
    }

    /*
    instance Alternative Parser where
        -- The identity of '<|>'
        empty :: Parser a
        empty = P (\inp -> Nothing)

        -- An associative binary operation
        (<|>) :: Parser a -> Parser a -> Parser a
        p <|> q = P (\inp -> parse p inp <|> parse q inp)
    */
    static Parser empty(){
        return Parser([](std::string const&){ return result_t(); });
    }

    Parser or_else(Parser const& p) const
    {
        Parser const self(*this);
        return Parser([self, p](std::string const& inp){
            return self.parse(inp).or_else([&p, &inp](){
                return p.parse(inp); // f is invoked only if parse failed.
            });
        });
    }

    Parser<std::string> some() const;
    Parser<std::string> many() const;

    Parser token() const;

    Parser rest_l(A a, Parser<bin_func_t> const& op) const;
    Parser chainl1(Parser<bin_func_t> const& op) const;
    Parser scan(Parser<bin_func_t> const& op) const;
    Parser rest_r(A a, Parser<bin_func_t> const& op) const;
    Parser chainr1(Parser<bin_func_t> const& op) const;

private:
    function_t p;
};

// Functor
template<typename A, typename B>
Parser<B> operator/(std::function<B(A const&)> const& f, Parser<A> const& p){
    return p.transform(f);
}

template<typename A, typename B>
Parser<B> operator/(std::function<B(A)> const& f, Parser<A> const& p){
    return p.transform(_([f](A const& a){ return f(a); }));
}

template<typename B, typename A, typename... Args>
Parser<std::function<B(Args...)> > operator/(std::function<B(A const&, Args...)> const& f, Parser<A> const& p){
    return p.transform(_([f](A const& a){
        return _([f, a](Args... args){
            return f(a, args...);
        });
    }));
}

// Monad
template<typename A, typename B>
Parser<B> operator>>=(Parser<A> const& p, std::function<Parser<B>(A const&)> const& f){
    return p.and_then(f);
}

template<typename A, typename B>
Parser<B> operator>>(Parser<A> const& p, Parser<B> const& q){
    return p >>= _([q](A const&){ return q; });
}

template<typename A, typename B>
Parser<B> operator>>(Parser<A> const& p, supplier<Parser<B> > const& q){
    return p >>= _([q](A const&){ return q(); });
}

#define _do(var, mexpr, expr) \
    ((mexpr) >>= _([=](typename std::decay_t<decltype(mexpr)>::value_type const& var){ return expr; }))

#define _do2(var1, mexpr1, var2, mexpr2, expr) \
    _do(var1, mexpr1, _do(var2, mexpr2, expr))

// Applicative
// (<*>) :: Parser (a -> b) -> Parser a -> Parser b
// pf <*> q = do f <- pf; f <$> q
template<typename A, typename B>
Parser<B> operator*(Parser<std::function<B(A const&)> > const& pf, supplier<Parser<A> > const& q){
    // q is invoked only if pf parsed successfully.
    return _do(f, pf, f / q());
}

template<typename A, typename B>
Parser<B> operator*(Parser<std::function<B(A)> > const& pf, supplier<Parser<A> > const& q){
    // q is invoked only if pf parsed successfully.
    return _do(f, pf, f / q());
}

// Alternative
template<typename A>
Parser<A> operator|(Parser<A> const& p, Parser<A> const& q){
    return p.or_else(q);
}

/*
-- -----------------------------------------------------------------------------
-- One or more.
some :: Alternative f => f a -> f [a]
some v = liftA2 (:) v (many v)

-- Zero or more.
many :: Alternative f => f a -> f [a]
many v = some v <|> pure []
*/

template<typename A>
Parser<std::string> Parser<A>::some() const {
    static_assert(std::is_same_v<A, char>);
    // make many lasy.
    Parser const self(*this);
    return _([](char const& c, std::string const& s){ return std::move(c + s); })
        / *this * _([self](){ return self.many(); });
}

template<typename A>
Parser<std::string> Parser<A>::many() const {
    static_assert(std::is_same_v<A, char>);
    return some() | Parser<std::string>::pure(std::string());
}

Parser<char> const anyChar([](std::string const& inp){
    return !inp.empty() ?
        Parser<char>::result_t(std::make_pair(inp.front(), inp.substr(1))) :
        Parser<char>::result_t();
});

Parser<char> satisfy(predicate<char> const& f){
    return _do(c, anyChar, f(c) ? Parser<char>::pure(c) : Parser<char>::empty());
}

Parser<char> const alnum = satisfy([](char c){ return (bool)std::isalnum(c) || c == '_'; });

Parser<char> symbol(char x){
    return satisfy([x](char c){ return c == x; }).token();
}

Parser<std::string> const spaces = satisfy([](char c){ return (bool)std::isspace(c); }).many();

template<typename A>
Parser<A> Parser<A>::token() const {
    return _do(a, spaces >> *this,
                  spaces >> pure(a));
}

Parser<double> const _double = Parser<double>([](std::string const& inp)
    {
        char *end = nullptr;
        double const d = std::strtod(inp.c_str(), &end);
        return !end || end == inp.c_str() || errno == ERANGE ?
            Parser<double>::result_t() :
            Parser<double>::result_t(Parser<double>::pair_t(d, end));
    }).token();

Parser<std::string> _name(std::string const& n){
    return _do(s, alnum.some(), s == n ? Parser<std::string>::pure(s) : Parser<std::string>::empty());
}

/*
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do { x <- p; rest x }
  where
    rest x = do { f <- op;
                  y <- p;
                  rest (f x y)
                }
          <|> return x
*/

template<typename A>
Parser<A> rest(A x, Parser<binary_function<A> > const& op, supplier<Parser<A> > const& fval, std::function<Parser<A>(A)> ff){
    return _do2(f, op, y, fval(), ff(f(x, y))) | Parser<A>::pure(x);
}

template<typename A>
Parser<A> Parser<A>::rest_l(A x, Parser<bin_func_t> const& op) const {
    Parser const self(*this);
    return rest(x, op, _([self](){ return self; }), _([self, op](A y){ return self.rest_l(y, op); }));
}

template<typename A>
Parser<A> Parser<A>::chainl1(Parser<bin_func_t> const& op) const {
    Parser const self(*this);
    return _do(x, *this, self.rest_l(x, op));
}

/*
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan   = do { x <- p; rest x }

    rest x = do { f <- op;
                  y <- scan;
                  return (f x y)
                }
          <|> return x
*/
template<typename A>
Parser<A> Parser<A>::scan(Parser<bin_func_t> const& op) const {
    Parser const self(*this);
    return _do(x, *this, self.rest_r(x, op));
}

template<typename A>
Parser<A> Parser<A>::rest_r(A x, Parser<bin_func_t> const& op) const {
    Parser const self(*this);
    return rest(x, op, _([self, op](){ return self.scan(op); }), _([](A y){ return pure(y); }));
}

template<typename A>
Parser<A> Parser<A>::chainr1(Parser<bin_func_t> const& op) const {
    return scan(op);
}

class calculator
{
private:
    using bin_func_t = binary_function<double>;
    using Parser_f = Parser<bin_func_t>;
    using Parser_d = Parser<double>;
    using func_t = unary_function<double>;
    using Parser_func = Parser<func_t>;

    Parser_d expr_in_brackets() const
    {
        // make expr and pure lasy
        calculator const self(*this);
        return _do(e, symbol('(') >> _([self](){ return self.expr(); }),
                      symbol(')') >> _([e](){ return Parser_d::pure(e); }));
    }

    Parser_d factor0() const
    {
        calculator const self(*this);
        return expr_in_brackets()
            // make expr_in_brackets lasy
            | func * _([self](){ return self.expr_in_brackets(); })
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

private:
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

    std::array<Parser_func, 14> const functions {
        FUNC(sin), FUNC(cos), FUNC(asin), FUNC(acos),
        FUNC(sinh), FUNC(cosh), FUNC(asinh), FUNC(acosh),
        FUNC(tan), FUNC(log), FUNC(log10), FUNC(exp),
        FUNC(sqrt), FUNC(sqr)
    };
    #undef FUNC

    Parser_func const func = std::reduce(functions.cbegin(), functions.cend(), Parser_func::empty(),
        [](Parser_func const& f1, Parser_func const& f2){ return f1 | f2; }).token();

    #define CONST(name) def_object(#name, M_##name)

    std::array<Parser_d, 13> const constants {
        CONST(E),        // 2.71828182845904523536   // e
        CONST(LOG2E),    // 1.44269504088896340736   // log2(e)
        CONST(LOG10E),   // 0.434294481903251827651  // log10(e)
        CONST(LN2),      // 0.693147180559945309417  // ln(2)
        CONST(LN10),     // 2.30258509299404568402   // ln(10)
        CONST(PI),       // 3.14159265358979323846   // pi
        CONST(PI_2),     // 1.57079632679489661923   // pi/2
        CONST(PI_4),     // 0.785398163397448309616  // pi/4
        CONST(1_PI),     // 0.318309886183790671538  // 1/pi
        CONST(2_PI),     // 0.636619772367581343076  // 2/pi
        CONST(2_SQRTPI), // 1.12837916709551257390   // 2/sqrt(pi)
        CONST(SQRT2),    // 1.41421356237309504880   // sqrt(2)
        CONST(SQRT1_2)   // 0.707106781186547524401  // 1/sqrt(2)
    };
    #undef CONST

    Parser_d const _const  = std::reduce(constants.cbegin(), constants.cend(), Parser_d::empty(),
        [](Parser_d const& c1, Parser_d const& c2){ return c1 | c2; }).token();
};

int main()
{
    std::cout << calculator().expr()
        .parse("7.21e1 - 7.3 - (1.5 - 2.2) * (-3.3)")
        //.parse("sqrt(exp(E * sin(2.2 * 2_PI)))")
        //.parse("sqr(2_PI)")
        //.parse("sqr(sin(2)) + sqr(cos(2))")
        //.parse("7-1-2")
        //.parse("3^2^3")
        //.parse("E^PI")
        .transform([](parser_pair<double> const& p)
    {
        assert(p.second.empty());
        return p.first;
    }).value() << std::endl;
}
