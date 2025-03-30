#pragma once

#include "func.hpp"

#include <optional>
#include <iostream>

template<typename T>
using Maybe = std::optional<T>;

template<typename T>
Maybe<T> Just(T const& t){
    return t;
}

constexpr std::nullopt_t Nothing = std::nullopt;

template<typename T>
constexpr Maybe<T> _Nothing = Nothing;

// Functor
template<typename A, typename B>
Maybe<B> operator/(std::function<B(A)> const& f, Maybe<A> const& x){
    return x.transform(f);
}

template<typename A, typename B>
Maybe<B> operator/(std::function<B(A const&)> const& f, Maybe<A> const& x){
    return x.transform(f);
}

// Applicative
template<typename A, typename B>
Maybe<B> operator*(Maybe<std::function<B(A)> > const& mf, Maybe<A> const& x){
    return mf.and_then(_([&x](std::function<B(A)> const& f){ return f / x; }));
}

template<typename A, typename B>
Maybe<B> operator*(Maybe<std::function<B(A const&)> > const& mf, Maybe<A> const& x){
    return mf.and_then(_([&x](std::function<B(A)> const& f){ return f / x; }));
}

template<typename A, typename B, typename C>
std::function<Maybe<C>(Maybe<A> const&, Maybe<B> const&)> Maybe_liftA2(std::function<C(A, B)> const f){
    return _([f](Maybe<A> const& mx, Maybe<B> const& my){
        return _([f](A const& a){ return _([f, a](B const& b){ return f(a, b); }); }) / mx * my;
    });
}

// Monad
template<typename A, typename B>
Maybe<B> operator>>=(Maybe<A> const& x, std::function<Maybe<B>(A)> const& f){
    return x.and_then(f);
}

template<typename A, typename B>
Maybe<B> operator>>=(Maybe<A> const& x, std::function<Maybe<B>(A const&)> const& f){
    return x.and_then(f);
}

template<typename A, typename B>
Maybe<B> operator>>(Maybe<A> const& x, Maybe<B> const& y){
    return x.and_then(_([&y](A const&){ return y; }));
}

template<typename A, typename B>
Maybe<B> operator>>(Maybe<A> const& x, supplier<Maybe<B> > const& fy){
    return x.and_then(_([&fy](A const&){ return fy(); }));
}

// Alternative
template<typename A>
Maybe<A> operator|(Maybe<A> const& p, supplier<Maybe<A> > const& q){
    return p.or_else(q);
}

template<typename A>
Maybe<A> operator|(Maybe<A> const& p, Maybe<A> const& q){
    return p | _([q](){ return q; });
}

template<typename A, typename B>
std::ostream& operator<<(std::ostream& os, std::pair<A, B> const& p){
    return os << '[' << p.first << ',' << p.second << ']';
}

template<typename A>
std::ostream& operator<<(std::ostream& os, std::optional<A> const& x){
    return x.has_value() ? os << '[' << x.value() << ']' : os << "[]";
}

inline std::ostream& operator<<(std::ostream& os, std::nullopt_t const&){
    return os << "[]";
}

#ifdef BOOST_TEST_DECL

// Boost Test support
namespace boost { namespace test_tools { namespace tt_detail {

template<typename A>
struct BOOST_TEST_DECL print_log_value<std::optional<A> >{
    void operator()( std::ostream& os, std::optional<A> const& x){
        ::operator<<(os, x);
    }
};                                                          

template<>
struct BOOST_TEST_DECL print_log_value<std::nullopt_t>{
    void operator()( std::ostream& os, std::nullopt_t const& x){
        ::operator<<(os, x);
    }
};                                                          

}}}

#endif // BOOST_TEST_DECL
