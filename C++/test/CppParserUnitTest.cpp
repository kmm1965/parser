#define BOOST_TEST_MODULE Parser Test
#ifdef _WIN32
#include <boost/test/unit_test.hpp>
#else
#include <boost/test/included/unit_test.hpp>
#endif

using namespace std::string_literals;

#include "test_Maybe.h"
#include "test_Parser.h"
#include "test_SomeParsers.h"
#include "test_Calculator.h"
