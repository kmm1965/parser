library(testthat)

source("../src/some_parsers.R")

test_empty_string <- function() {
  test_that("Testing empty_string",
    {
      expect_equal(empty_string %>% parse("abc"), just(list("", "abc")))
    })
}

test_any_char <- function() {
  test_that("Testing any_char",
    {
      expect_equal(any_char %>% parse("abc"), just(list('a', "bc")))
      expect_equal(any_char %>% parse(""), nothing())
    })
}

test_satisfy <- function() {
  test_that("Testing satisfy",
    {
      expect_equal(satisfy(function(c){ c == 'a' }) %>% parse("abc"), just(list('a', "bc")))
      expect_equal(satisfy(function(c){ c == 'z' }) %>% parse("abc"), nothing())
      expect_equal(satisfy(function(c){ c == 'a' }) %>% parse(""), nothing())
    })
}

test_char <- function() {
  test_that("Testing char",
    {
      expect_equal(char('a') %>% parse("abc"), just(list('a', "bc")))
      expect_equal(char('z') %>% parse("abc"), nothing())
      expect_equal(char('a') %>% parse(""), nothing())
    })
}

test_spaces <- function() {
  test_that("Testing char",
    {
      expect_equal(spaces %>% parse("abc"), just(list("", "abc")))
      expect_equal(spaces %>% parse("   abc"), just(list("   ", "abc")))
    })
}

test_symbol <- function() {
  test_that("Testing symbol",
    {
      expect_equal(symbol('+') %>% parse(" + abc"), just(list('+', "abc")))
      expect_equal(symbol('+') %>% parse("abc"), nothing())
    })
}

test_alnum <- function() {
  test_that("Testing alnum",
    {
      expect_equal(alnum %>% parse("123abc"), just(list('1', "23abc")))
      expect_equal(alnum %>% parse("abc"), just(list('a', "bc")))
      expect_equal(alnum %>% parse("_123abc"), just(list('_', "123abc")))
      expect_equal(alnum %>% parse("!@#"), nothing())
    })
}

test_sign <- function() {
  test_that("Testing sign",
    {
      expect_equal(sign %>% parse("abc"), just(list("", "abc")))
      expect_equal(sign %>% parse("+abc"), just(list("+", "abc")))
      expect_equal(sign %>% parse("-abc"), just(list("-", "abc")))

      expect_equal(usign %>% parse("abc"), just(list("", "abc")))
      expect_equal(usign %>% parse(" + abc"), just(list("+", "abc")))
      expect_equal(usign %>% parse(" - abc"), just(list("-", "abc")))
    })
}

test_digits <- function() {
  test_that("Testing digits",
    {
      expect_equal(digits %>% parse("123abc"), just(list("123", "abc")))
      expect_equal(digits %>% parse("123  abc"), just(list("123", "  abc")))
      expect_equal(digits %>% parse("abc"), just(list("", "abc")))
    })
}

test_double <- function() {
  test_that("Testing double",
    {
      expect_equal(double %>% parse("1 abc"), just(list(1.0, "abc")))
      expect_equal(double %>% parse("1. abc"), just(list(1.0, "abc")))
      expect_equal(double %>% parse("1.23 abc"), just(list(1.23, "abc")))
      expect_equal(double %>% parse("-1.23 abc"), nothing())
      expect_equal(double %>% parse(".23 abc"), just(list(0.23, "abc")))
      expect_equal(double %>% parse(" + 1.23 abc"), nothing())
      expect_equal(double %>% parse("1.23e10abc"), just(list(1.23e10, "abc")))
      expect_equal(double %>% parse("1.23e-10abc"), just(list(1.23e-10, "abc")))
      expect_equal(double %>% parse("abc"), nothing())
    })
}

test_between <- function() {
  test_that("Testing between",
    {
      expr <- between(symbol('('), symbol(')'), function(){ double })

      expect_equal(expr %>% parse(" ( 123 ) abc"), just(list(123.0, "abc")))
      expect_equal(expr %>% parse(" ( 123 abc"), nothing())
      expect_equal(expr %>% parse(" 123 ) abc"), nothing())
    })
}

test_chainlr1 <- function() {
  test_that("Testing chainlr1",
    {
      add <- symbol('+') %>% parser_skip(function(){ pure(function(x, y){ x + y }) })
      sub <- symbol('-') %>% parser_skip(function(){ pure(function(x, y){ x - y }) })
      pow <- symbol('^') %>% parser_skip(function(){ pure(function(x, y){ exp(y * log(x)) }) })

      pexpr <- chainl1(double, add %>% parser_or_else(function() { sub }), FALSE)

      expect_equal(pexpr %>% parse("7abc"), just(list(7.0, "abc")))
      expect_equal(pexpr %>% parse(" 7 - 1 - 2 abc"), just(list(4.0, "abc")))
      expect_equal(pexpr %>% parse(" 7 - 1 + 2 - 3 abc"), just(list(5.0, "abc")))
      expect_equal(pexpr %>% parse("abc"), nothing())

      expect_equal(chainr1(double, pow) %>% parse("3 ^ 2 ^ 3 abc") %>%
        fmap(function(p) { list(round(p[[1]] + 0.5), p[[2]]) }),
        just(list(6561.0, "abc")))
    })
}

test_empty_string()
test_any_char()
test_satisfy()
test_char()
test_spaces()
test_symbol()
test_alnum()
test_sign()
test_digits()
test_double()
test_between()
test_chainlr1()
