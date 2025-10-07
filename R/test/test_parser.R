library(testthat)

source("../src/parser.R")

test_parser_pure <- function() {
  test_that("Testing parser pure",
    {
      expect_equal(pure(1) %>% parse("abc"), just(list(1, "abc")))
      expect_equal(pure(1.0) %>% parse("abc"), just(list(1.0, "abc")))
      expect_equal(pure("1") %>% parse("abc"), just(list("1", "abc")))
    })
}

test_parser_map <- function() {
  test_that("Testing parser map",
    {
      expect_equal(pure(1) %>% parser_map(as.character) %>% parse("abc"), just(list("1", "abc")))
      expect_equal(pure(1.0) %>% parser_map(as.character) %>% parse("abc"), just(list("1", "abc")))
      expect_equal(pure("1") %>% parser_map(as.integer) %>% parse("abc"), just(list(1, "abc")))

      expect_equal(empty %>% parser_map(as.character) %>% parse("abc"), nothing())
      expect_equal(empty %>% parser_map(as.integer) %>% parse("abc"), nothing())
    })
}

test_parser_and_then <- function() {
  test_that("Testing parser and_then",
    {
      eat <- function(i) { as_parser(function(inp) { just(list(paste0(as.character(i), inp), "")) }) }
      cancel <- function(i) { empty }
      i1 <- pure(1)

      expect_equal(i1 %>% parser_and_then(eat) %>% parse("abc"), just(list("1abc", "")))
      expect_equal(i1 %>% parser_and_then(cancel) %>% parse("abc"), nothing())
      expect_equal(empty %>% parser_and_then(eat) %>% parse("abc"), nothing())
      expect_equal(empty %>% parser_and_then(cancel) %>% parse("abc"), nothing())
    })
}

test_parser_apply <- function() {
  test_that("Testing parser apply",
    {
      psin <- pure(sin)
      f1 <- function() { pure(1.0) }
      fe <- function() { empty }

      expect_equal(psin %>% parser_apply(f1) %>% parse("abc"), just(list(sin(1.0), "abc")))
      expect_equal(psin %>% parser_apply(fe) %>% parse("abc"), nothing())
      expect_equal(empty %>% parser_apply(f1) %>% parse("abc"), nothing())
      expect_equal(empty %>% parser_apply(fe) %>% parse("abc"), nothing())
    })
}

test_parser_pure()
test_parser_map()
test_parser_and_then()
test_parser_apply()
