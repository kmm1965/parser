#include "SomeParsers.hpp"

Parser<char> const anyChar = _([](std::string const& inp){
    return !inp.empty() ?
        Parser<char>::result_t(std::make_pair(inp.front(), inp.substr(1))) :
        Parser<char>::result_t();
});

Parser<char> const alnum = satisfy([](char c){ return (bool)std::isalnum(c) || c == '_'; });

Parser<std::string> const spaces = satisfy([](char c){ return (bool)std::isspace(c); }).many();

Parser<double> const _double = Parser<double>([](std::string const& inp)
{
    char *end = nullptr;
    double const d = std::strtod(inp.c_str(), &end);
    return !end || end == inp.c_str() || errno == ERANGE ?
        Parser<double>::result_t() :
        Parser<double>::result_t(Parser<double>::pair_t(d, end));
}).token();
