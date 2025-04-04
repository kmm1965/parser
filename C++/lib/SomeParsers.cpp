#include "SomeParsers.hpp"

Parser<char> const alnum = satisfy([](char c){ return (bool)std::isalnum(c) || c == '_'; });

Parser<double> const double_ = ~Parser<double>([](std::string const& inp)
{
    char *end = nullptr;
    double const d = std::strtod(inp.c_str(), &end);
    return end && end != inp.c_str() && errno != ERANGE ?
        Just(Parser<double>::pair_t(d, end)) :
        Nothing;
});

Parser<char> char_(char c){
    return satisfy(_([c](char x){ return x == c; }));
}

Parser<char> symbol(char c){
    return ~char_(c);
}

Parser<std::string> name_(std::string const& n){
    return ~_do(s, +alnum, s == n ? Parser<std::string>::pure(n) : Parser<std::string>::empty());
}
