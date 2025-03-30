#pragma once

#include "../lib/Maybe.hpp"

BOOST_AUTO_TEST_CASE(test_Maybe_Functor)
{
    unary_function<double> const fsin = [](double x){ return std::sin(x); };
    Maybe<double> const dNothing;

    BOOST_TEST(Just(1.).transform(fsin) == Just(std::sin(1)));
    BOOST_TEST(dNothing.transform(fsin) == Nothing);

    //BOOST_TEST(fsin / Just(1.) == Just(std::sin(1)));
    //BOOST_TEST(fsin / dNothing == Nothing);

    BOOST_TEST(operator/(fsin, Just(1.)) == Just(std::sin(1)));
    BOOST_TEST(operator/(fsin, dNothing) == Nothing);

    auto const to_s = [](int x){ return std::to_string(x); };
    BOOST_TEST(Just(1).transform(to_s) == Just("1"s));
    BOOST_TEST(_Nothing<int>.transform(to_s) == Nothing);
}

BOOST_AUTO_TEST_CASE(test_Maybe_Applicative)
{
    binary_function2<Maybe<int> > const fminus = Maybe_liftA2(_([](int x, int y){ return x - y; }));
    std::function<unary_function<int>(int)> const minus = [](int x){ return _([x](int y){ return x - y; }); };
    Maybe<int> const iNothing;

    BOOST_TEST(fminus(Just(8), Just(3)) == Just(5));
    BOOST_TEST(fminus(iNothing, Just(8)) == Nothing);
    BOOST_TEST(fminus(Just(8), iNothing) == Nothing);
    BOOST_TEST(fminus(iNothing, iNothing) == Nothing);

    //BOOST_TEST(minus / Just(8) * Just(3) == Just(5));

    BOOST_TEST(operator*<int>(Just(8).transform(minus), Just(3)) == Just(5));
    BOOST_TEST(operator*<int>(iNothing.transform(minus), Just(3)) == Nothing);
    BOOST_TEST(operator*<int>(Just(8).transform(minus), iNothing) == Nothing);
    BOOST_TEST(operator*<int>(iNothing.transform(minus), iNothing) == Nothing);
}

BOOST_AUTO_TEST_CASE(test_Maybe_Monad)
{
    std::function<Maybe<double>(double)> const
        safe_sqrt = [](double x){ return x >= 0 ? Just(std::sqrt(x)) : Nothing; },
        safe_log =  [](double x){ return x >  0 ? Just(std::log (x)) : Nothing; };

    Maybe<double> const dNothing;
    Maybe<int> const iNothing;

    BOOST_TEST(safe_sqrt(2) == Just(std::sqrt(2)));
    BOOST_TEST(safe_sqrt(2).and_then(safe_log) == Just(std::log(std::sqrt(2))));
    BOOST_TEST(safe_sqrt(0).and_then(safe_log) == Nothing);
    BOOST_TEST(safe_sqrt(-2).and_then(safe_log) == Nothing);

    BOOST_TEST((safe_sqrt(2) >>= safe_log) == Just(std::log(std::sqrt(2))));
    BOOST_TEST((safe_sqrt(0) >>= safe_log) == Nothing);
    BOOST_TEST((safe_sqrt(-2) >>= safe_log) == Nothing);

    BOOST_TEST(Just(2).and_then(safe_sqrt) == Just(std::sqrt(2)));
    BOOST_TEST(Just(2).and_then(safe_sqrt).and_then(safe_log) == Just(std::log(std::sqrt(2))));
    BOOST_TEST(Just(0).and_then(safe_log) == Nothing);
    BOOST_TEST(Just(-2).and_then(safe_sqrt) == Nothing);

    BOOST_TEST((Just(2.) >>= safe_sqrt) == Just(std::sqrt(2)));
    BOOST_TEST(((Just(2.) >>= safe_sqrt) >>= safe_log) == Just(std::log(std::sqrt(2))));
    BOOST_TEST((Just(0.) >>= safe_log) == Nothing);
    BOOST_TEST((Just(-2.) >>= safe_sqrt) == Nothing);

    BOOST_TEST(dNothing.and_then(safe_sqrt) == Nothing);
    BOOST_TEST(dNothing.and_then(safe_sqrt).and_then(safe_log) == Nothing);

    BOOST_TEST((dNothing >>= safe_sqrt) == Nothing);
    BOOST_TEST(((dNothing >>= safe_sqrt) >>= safe_log) == Nothing);

    std::function<Maybe<std::string>(int)> const fs = [](int x){ return x % 2 == 0 ? Just(std::to_string(x)) : Nothing; };
    BOOST_TEST(Just(2).and_then(fs) == Just("2"s));
    BOOST_TEST(Just(1).and_then(fs) == Nothing);
    BOOST_TEST(iNothing.and_then(fs) == Nothing);

    BOOST_TEST((Just(2) >>= fs) == Just("2"s));
    BOOST_TEST((Just(1) >>= fs) == Nothing);
    BOOST_TEST((iNothing >>= fs) == Nothing);
}

BOOST_AUTO_TEST_CASE(test_Maybe_Alternative)
{
    std::function<Maybe<int>()> const
        fJust = [](){ return Just(3); },
        fNothing = [](){ return Nothing; };

    Maybe<int> const iNothing;

    BOOST_TEST(Just(2).or_else(fJust) == Just(2));
    BOOST_TEST(Just(2).or_else(fNothing) == Just(2));

    BOOST_TEST(iNothing.or_else(fJust) == Just(3));
    BOOST_TEST(iNothing.or_else(fNothing) == Nothing);

    BOOST_TEST((Just(2) | fJust) == Just(2));
    BOOST_TEST((Just(2) | fNothing) == Just(2));

    BOOST_TEST((iNothing | fJust) == Just(3));
    BOOST_TEST((iNothing | fNothing) == Nothing);
}
