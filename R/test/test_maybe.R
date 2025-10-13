library(testthat)
library(maybe)

test_maybe_map <- function() {
  test_that("Testing maybe_map",
    {
      expect_equal(just(1.0) %>% fmap(sin), just(sin(1.0)))
      expect_equal(nothing() %>% fmap(sin), nothing())
      expect_equal(just(1) %>% fmap(as.character), just("1"))
      expect_equal(nothing() %>% fmap(as.character), nothing())
    })
}

test_maybe_and_then <- function() {
  safe_sqrt <- function(x) { maybe(sqrt, ensure = not_infinite)(x) }
  safe_log <- function(x) { maybe(log, ensure = not_infinite)(x) }
  toString <- function(i) { if(i %% 2 == 0){ just(as.character(i))} else { nothing() }}
    
  test_that("Testing maybe_and_then",
    {
      expect_equal(safe_sqrt(2.0), just(sqrt(2.0)))
      expect_equal(safe_sqrt(0.0), just(0.0))
      expect_equal(safe_sqrt(-2.0), nothing())

      expect_equal(safe_log(2.0), just(log(2.0)))
      expect_equal(safe_log(0.0), nothing())
      expect_equal(safe_log(-2.0), nothing())

      expect_equal(just(2.0) %>% and_then(safe_sqrt), just(sqrt(2.0)))
      expect_equal(just(0.0) %>% and_then(safe_sqrt), just(0.0))
      expect_equal(just(-2.0) %>% and_then(safe_sqrt), nothing())

      expect_equal(just(2.0) %>% and_then(safe_sqrt) %>% and_then(safe_log), just(log(sqrt(2.0))))
      expect_equal(just(0.0) %>% and_then(safe_sqrt) %>% and_then(safe_log), nothing())
      expect_equal(just(-2.0) %>% and_then(safe_sqrt) %>% and_then(safe_log), nothing())

      expect_equal(just(2) %>% and_then(toString), just("2"))
      expect_equal(just(1) %>% and_then(toString), nothing())
      expect_equal(nothing() %>% and_then(toString), nothing())
    })
}

test_maybe_or_else <- function() {
  or_else <- function(m, f){ if(is_just(m)){ m } else f() }
  f2 <- function(){ just(2.0) }

  test_that("Testing maybe_or_else",
    {
      expect_equal(just(1.0) %>% or_else(f2), just(1.0))
      expect_equal(nothing() %>% or_else(f2), just(2.0))
    })
}

test_maybe_map()
test_maybe_and_then()
test_maybe_or_else()
