#pragma once

#include "Parser.hpp"

extern Parser<char> const alnum;
extern Parser<double> const _double;

Parser<char> _char(char c);
Parser<char> symbol(char c);
Parser<std::string> _name(std::string const& n);
