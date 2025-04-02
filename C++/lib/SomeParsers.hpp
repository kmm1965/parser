#pragma once

#include "Parser.hpp"

extern Parser<char> const alnum;
extern Parser<double> const double_;

Parser<char> char_(char c);
Parser<char> symbol(char c);
Parser<std::string> name_(std::string const& n);

inline Parser<char> operator ""_l(char c){
    return char_(c);
}

inline Parser<char> operator ""_s(char c){
    return symbol(c);
}

inline Parser<std::string> operator ""_n(const char *s, std::size_t){
    return name_(s);
}
