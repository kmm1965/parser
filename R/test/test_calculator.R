library(testthat)

source("../src/calculator.R")

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

test_calculator()
