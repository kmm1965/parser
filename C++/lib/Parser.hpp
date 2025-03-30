#pragma once

#include "Maybe.hpp"

#include <string>

template<typename T>
using parser_pair = std::pair<T, std::string>;

template<typename T>
using parser_data = Maybe<parser_pair<T> >;

template<typename A>
struct Parser {
    using value_type = A;
    using pair_t = parser_pair<A>;
    using result_t = parser_data<A>;
    using function_t = std::function<result_t(std::string const&)>;
    using bin_func_t = binary_function<A>;

    Parser(function_t const& unp) : unp(unp){}

    result_t parse(std::string const& inp) const {
        return unp(inp);
    }

    // instance Functor Parser where
    //    fmap :: (a -> b) -> Parser a -> Parser b
    //    fmap f p = P (\inp -> (\(x, out) -> (f x, out)) <$> parse p inp)
    template<typename B>
    Parser<B> transform(std::function<B(A const&)> const& f) const {
        return Parser<B>([self = *this, f](std::string const& inp){
            return self.parse(inp).transform([f](pair_t const& p){
                return std::make_pair(f(p.first), p.second);
            });
        });
    }

    template<typename B>
    Parser<B> transform(std::function<B(A)> const& f) const {
        return transform(_([f](A const& a){ return f(a); }));
    }

    /*
    instance Applicative Parser where
        pure :: a -> Parser a
        pure f = P ( \inp -> Just (f, inp))
    */
    static Parser pure(A const& a){
        return Parser([a](std::string const& inp){
            return Just(pair_t(a, inp));
        });
    }

    /*
    instance Monad Parser where
        (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= f = P (\inp -> parse p inp >>= (\(x, out) -> parse (f x) out))
    */
    template<typename B>
    Parser<B> and_then(std::function<Parser<B>(A const&)> const& f) const {
        return Parser<B>([self = *this, f](std::string const& inp){
            return self.parse(inp).and_then([f](pair_t const& p){
                return f(p.first).parse(p.second);
            });
        });
    }

    template<typename B>
    Parser<B> and_then(std::function<Parser<B>(A)> const& f) const {
        return and_then(_([f](A const& a){ return f(a); }));
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
        return Parser([](std::string const&){ return Nothing; });
    }

    Parser<std::string> some() const;
    Parser<std::string> many() const;

    Parser token() const;

    Parser rest_l(Parser<bin_func_t> const& op, A const& a) const;
    Parser rest_r(Parser<bin_func_t> const& op, A const& a) const;
    Parser chainl1(Parser<bin_func_t> const& op) const;
    Parser chainr1(Parser<bin_func_t> const& op) const;

private:
    function_t unp;
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
Parser<B> operator>>=(Parser<A> const& p, std::function<Parser<B>(A)> const& f){
    return p.and_then(f);
}

template<typename A, typename B>
Parser<B> operator>>(Parser<A> const& p, supplier<Parser<B> > const& q){
    return p >>= _([q](A const&){ return q(); });
}

template<typename A, typename B>
Parser<B> operator>>(Parser<A> const& p, Parser<B> const& q){
    return p >> _([q](){ return q; });
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
    return _do(f, pf, f / q());
}

// Alternative
template<typename A>
Parser<A> operator|(Parser<A> const& p, supplier<Parser<A> > const& q){
    return Parser<A>([p, q](std::string const& inp){
        return p.parse(inp) | _([&q, &inp](){
            return q().parse(inp); // q.parse is invoked only if p.parse failed.
        });
    });
}

template<typename A>
Parser<A> operator|(Parser<A> const& p, Parser<A> const& q){
    return p | _([q](){ return q; });
}

template<typename Open, typename Close, typename A>
Parser<A> between(Parser<Open> const& open, Parser<Close> const& close, supplier<Parser<A> > const& fp)
{
    // make expr and pure lasy
    return _do(e, open >> _([fp](){ return fp(); }),
        close >> _([e](){ return Parser<A>::pure(e); }));
}

extern Parser<char> const anyChar;
extern Parser<std::string> const spaces;

Parser<char> satisfy(predicate<char> const& f);

template<typename A>
Parser<A> Parser<A>::token() const {
    return between(spaces, spaces, _([self = *this](){ return self; }));
}

inline Parser<std::string> optional_s(Parser<std::string> const& p){
    return p | Parser<std::string>::pure(std::string());
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
    return _([](char const& c, std::string const& s){ return std::move(c + s); })
        / *this * _([self = *this](){ return self.many(); });
}

template<typename A>
Parser<std::string> Parser<A>::many() const {
    return optional_s(some());
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
Parser<A> rest(supplier<Parser<A> > const& fval, std::function<Parser<A>(A const&)> ff, Parser<binary_function<A> > const& op, A const& a){
    return _do2(f, op, b, fval(), ff(f(a, b))) | Parser<A>::pure(a);
}

template<typename A>
Parser<A> Parser<A>::rest_l(Parser<bin_func_t> const& op, A const& a) const {
    return rest(_([self = *this](){ return self; }), _([self = *this, op](A const& b){ return self.rest_l(op, b); }), op, a);
}

template<typename A>
Parser<A> Parser<A>::rest_r(Parser<bin_func_t> const& op, A const& a) const {
    return rest(_([self = *this, op](){ return self.chainr1(op); }), _([](A const& b){ return pure(b); }), op, a);
}

template<typename A>
Parser<A> Parser<A>::chainl1(Parser<bin_func_t> const& op) const {
    Parser const self = *this;
    return _do(a, *this, self.rest_l(op, a));
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
Parser<A> Parser<A>::chainr1(Parser<bin_func_t> const& op) const
{
    Parser const self = *this;
    return _do(a, *this, self.rest_r(op, a));
}
