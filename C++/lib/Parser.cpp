#include "Parser.hpp"

Parser<char> const anyChar = _([](std::string const& inp){
    return !inp.empty() ?
        Just(std::make_pair(inp.front(), inp.substr(1))) :
        Nothing;
});

Parser<char> satisfy(predicate<char> const& f) {
    return _do(c, anyChar, f(c) ? Parser<char>::pure(c) : Parser<char>::empty());
}

Parser<std::string> const spaces = satisfy([](char c) { return (bool)std::isspace(c); }).many();
