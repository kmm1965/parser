#pragma once

#include "Parser.hpp"

extern Parser<char> const anyChar;
extern Parser<char> const alnum;
extern Parser<double> const _double;

inline Parser<char> satisfy(predicate<char> const& f){
    return _do(c, anyChar, f(c) ? Parser<char>::pure(c) : Parser<char>::empty());
}

inline Parser<char> _char(char c){
    return satisfy(_([c](char x){ return x == c; }));
}

inline Parser<char> symbol(char c){
    return _char(c).token();
}

inline Parser<std::string> _name(std::string const& n){
    return _do(s, alnum.some(), s == n ? Parser<std::string>::pure(n) : Parser<std::string>::empty()).token();
}
