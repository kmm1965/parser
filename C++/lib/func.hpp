#pragma once

#include <functional>

template<typename F>
auto _(F const& f){
    return std::function(f);
}

template<typename T>
auto _(T(*f)(T)) {
    return std::function(f);
}

template<typename T>
using supplier = std::function<T()>;

template<typename T>
using unary_function = std::function<T(T)>;

template<typename T>
using unary_function2 = std::function<T(T const&)>;

template<typename T>
using binary_function = std::function<T(T, T)>;

template<typename T>
using binary_function2 = std::function<T(T const&, T const&)>;

template<typename T>
using predicate = std::function<bool(T)>;

template<typename T>
using predicate2 = std::function<bool(T const&)>;
