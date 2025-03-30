#pragma once

#include "../lib/Parser.hpp"

BOOST_AUTO_TEST_CASE(test_Parser_Functor)
{
    auto const i_to_s = _([](int x){ return std::to_string(x); });
    auto const d_to_s = _([](double x){ return std::to_string(x); });
    auto const s_to_i = _([](std::string const& s){ return std::stoi(s); });

    BOOST_TEST(Parser<int>::pure(1).transform(i_to_s).parse("abc") == Just(std::make_pair("1"s, "abc"s)));
    BOOST_TEST(Parser<double>::pure(1.0).transform(d_to_s).parse("abc") == Just(std::make_pair("1.000000"s, "abc"s)));
    BOOST_TEST(Parser<std::string>::pure("1").transform(s_to_i).parse("abc") == Just(std::make_pair(1, "abc"s)));

    BOOST_TEST(Parser<int>::empty().transform(i_to_s).parse("abc") == Nothing);
    BOOST_TEST(Parser<double>::empty().transform(d_to_s).parse("abc") == Nothing);
    BOOST_TEST(Parser<std::string>::empty().transform(s_to_i).parse("abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_Parser_Applicative_pure)
{
    BOOST_TEST(Parser<int>::pure(1).parse("abc") == Just(std::make_pair(1, "abc"s)));
    BOOST_TEST(Parser<double>::pure(1.0).parse("abc") == Just(std::make_pair(1.0, "abc"s)));
    BOOST_TEST(Parser<std::string>::pure("1").parse("abc") == Just(std::make_pair("1"s, "abc"s)));
}

BOOST_AUTO_TEST_CASE(test_Parser_Applicative_apply)
{
    supplier<Parser<double> > const
        p1 = [](){ return Parser<double>::pure(1); },
        pempty = [](){ return Parser<double>::empty(); };
    Parser<unary_function<double> > const
        psin = Parser<unary_function<double> >::pure(_([](double x){ return std::sin(x); })),
        pfempty = Parser<unary_function<double> >::empty();

    BOOST_TEST((psin * p1).parse("abc") == Just(std::make_pair(std::sin(1), "abc"s)));
    BOOST_TEST((psin * pempty).parse("abc") == Nothing);
    BOOST_TEST((pfempty * p1).parse("abc") == Nothing);
    BOOST_TEST((pfempty * pempty).parse("abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_Parser_Monad)
{
    Parser<int> const
        p1 = Parser<int>::pure(1),
        pempty = Parser<int>::empty();

    Parser<std::string> const ps = Parser<std::string>::pure("123");

    std::function<Parser<std::string>(int)> const
        eat = _([](int x){ return Parser<std::string>(_([x](std::string const& inp){ return Just(std::make_pair(std::to_string(x) + inp, ""s)); })); }),
        cancel = _([](int){ return Parser<std::string>(_([](std::string const&){ return Nothing; })); });

    BOOST_TEST(p1.and_then(eat).parse("abc") == Just(std::make_pair("1abc"s, ""s)));
    BOOST_TEST(p1.and_then(cancel).parse("abc") == Nothing);

    BOOST_TEST((p1 >>= eat).parse("abc") == Just(std::make_pair("1abc"s, ""s)));
    BOOST_TEST((p1 >>= cancel).parse("abc") == Nothing);

    BOOST_TEST((p1 >> ps).parse("abc") == Just(std::make_pair("123"s, "abc"s)));
    BOOST_TEST((p1 >> _([ps](){ return ps; })).parse("abc") == Just(std::make_pair("123"s, "abc"s)));
}

BOOST_AUTO_TEST_CASE(test_Parser_anyChar)
{
    BOOST_TEST(anyChar.parse("abc") == Just(std::make_pair('a', "bc"s)));
    BOOST_TEST(anyChar.parse("") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_Parser_satisfy)
{
    BOOST_TEST(satisfy(_([](char c){ return c == 'a'; })).parse("abc") == Just(std::make_pair('a', "bc"s)));
    BOOST_TEST(satisfy(_([](char c){ return c == 'z'; })).parse("abc") == Nothing);
}

BOOST_AUTO_TEST_CASE(test_Parser_spaces)
{
    BOOST_TEST(spaces.parse("abc") == Just(std::make_pair(""s, "abc"s)));
    BOOST_TEST(spaces.parse("  abc") == Just(std::make_pair("  "s, "abc"s)));
}
