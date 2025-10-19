library(testthat)

source("../src/calculator.R")

test_funcs <- function() {
  test_that("Testing funcs",
    {
      expect_equal(calculate("sin(2.0)"), just(list(sin(2.0), "")))
      expect_equal(calculate("cos(2.0)"), just(list(cos(2.0), "")))
      expect_equal(calculate("asin(0.5)"), just(list(asin(0.5), "")))
      expect_equal(calculate("acos(0.5)"), just(list(acos(0.5), "")))
      expect_equal(calculate("sinh(2.0)"), just(list(sinh(2.0), "")))
      expect_equal(calculate("cosh(2.0)"), just(list(cosh(2.0), "")))
      expect_equal(calculate("asinh(2.0)"), just(list(asinh(2.0), "")))
      expect_equal(calculate("acosh(2.0)"), just(list(acosh(2.0), "")))
      expect_equal(calculate("tan(2.0)"), just(list(tan(2.0), "")))
      expect_equal(calculate("log(2.0)"), just(list(log(2.0), "")))
      expect_equal(calculate("log10(2.0)"), just(list(log10(2.0), "")))
      expect_equal(calculate("exp(2.0)"), just(list(exp(2.0), "")))
      expect_equal(calculate("sqrt(2.0)"), just(list(sqrt(2.0), "")))
      expect_equal(calculate("sqr(2.0)"), just(list(4.0, "")))
    })
}

test_consts <- function() {
  test_that("Testing consts",
    {
      expect_equal(calculate("E"), just(list(2.7182818284590452, "")))
      expect_equal(calculate("LOG2E"), just(list(1 / log(2.0), "")))
      expect_equal(calculate("LOG10E"), just(list(0.4342944819032518, "")))
      expect_equal(calculate("LOG10E"), just(list(1 / log(10.0), "")))
      expect_equal(calculate("LN2"), just(list(log(2.0), "")))
      expect_equal(calculate("LN10"), just(list(log(10.0), "")))
      expect_equal(calculate("PI"), just(list(pi, "")))
      expect_equal(calculate("PI_2"), just(list(pi / 2, "")))
      expect_equal(calculate("PI_4"), just(list(pi / 4, "")))
      expect_equal(calculate("1_PI"), just(list(1 / pi, "")))
      expect_equal(calculate("2_PI"), just(list(2 / pi, "")))
      expect_equal(calculate("2_SQRTPI"), just(list(2 / sqrt(pi), "")))
      expect_equal(calculate("SQRT2"), just(list(sqrt(2), "")))
      expect_equal(calculate("SQRT1_2"), just(list(sqrt(0.5), "")))
    })
}

test_calculator <- function() {
  test_that("Testing calculator",
    {
      expect_equal(calculate("72 - 7 - (1 - 2) * 3"), just(list(68.0, "")))
      expect_equal(calculate("-72 - 7 - (1 - 2) * 3"), just(list(-76.0, "")))
      expect_equal(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"), just(list(-8.889, "")))
      expect_equal(calculate("3^(1+1)^3") %>% fmap(function(p){ list(round(p[[1]] + 0.5), p[[2]]) }), just(list(6561.0, "")))
      expect_equal(calculate("sin(1+1)"), just(list(sin(2.0), "")))
      expect_equal(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"), just(list(-0.3634085731426532, "")))
      expect_equal(calculate("sqr(2 + 3)"), just(list(25.0, "")))
      expect_equal(calculate("sin(-PI/4)"), just(list(sin(-pi/4.0), "")))
      expect_equal(calculate(" E ^ PI"), just(list(23.140692632779267, "")))
      expect_equal(calculate(" PI ^ E"), just(list(22.45915771836104, "")))
    })
}

test_funcs()
test_consts()
test_calculator()
