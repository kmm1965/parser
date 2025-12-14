source("../src/some_parsers.R")

op2 <- function(c, f){
  symbol(c) %>% parser_skip(function(){ pure(f) })
}

add <- op2('+', `+`)
sub <- op2('-', `-`)
mul <- op2('*', `*`)
div <- op2('/', `/`)
pow <- op2('^', function(x, y){ exp(y * log(x)) })

fold <- function(parsers){
  token(Reduce(function(p, q){
    p %>% parser_or_else(function(){ q })
  }, parsers))
}

guard <- function(b, value){
  if(b) pure(value) else empty
}

funcs <- identifier %>% parser_and_then(
  function(n){ fold(list(
    guard(n == "sin",   sin),
    guard(n == "cos",   cos),
    guard(n == "asin",  asin),
    guard(n == "acos",  acos),
    guard(n == "sinh",  sinh),
    guard(n == "cosh",  cosh),
    guard(n == "asinh", asinh),
    guard(n == "acosh", acosh),
    guard(n == "tan",   tan),
    guard(n == "log",   log),
    guard(n == "log10", log10),
    guard(n == "exp",   exp),
    guard(n == "sqrt",  sqrt),
    guard(n == "sqr",   function(x){ x * x })
  ))
})

consts <- identifier %>% parser_and_then(
  function(n){ fold(list(
    guard(n == "E",        2.7182818284590452),
    guard(n == "PI",       pi),
    guard(n == "LOG2E",    1.44269504088896340736),  # log2(e)
    guard(n == "LOG10E",   0.434294481903251827651), # log10(e)
    guard(n == "LN2",      0.693147180559945309417), # ln(2)
    guard(n == "LN10",     2.30258509299404568402),  # ln(10)
    guard(n == "PI_2",     1.57079632679489661923),  # pi/2
    guard(n == "PI_4",     0.785398163397448309616), # pi/4
    guard(n == "1_PI",     0.318309886183790671538), # 1/pi
    guard(n == "2_PI",     0.636619772367581343076), # 2/pi
    guard(n == "2_SQRTPI", 1.12837916709551257390),  # 2/sqrt(pi)
    guard(n == "SQRT2",    1.41421356237309504880),  # sqrt(2)
    guard(n == "SQRT1_2",  0.707106781186547524401)  # 1/sqrt(2)
  ))
})

expr <- function(){
  usign %>% parser_and_then(function(sgn){
    term() %>% chainl1(add %>% parser_or_else(function(){ sub }), sgn == '-')
  })
}

term <- function(){
  factor() %>% chainl1(mul %>% parser_or_else(function(){ div }), FALSE)
}

factor <- function(){
    factor0() %>% chainr1(pow)
}

factor0 <- function(){
  expr_in_brackets() %>%
    parser_or_else(function(){ funcs %>% parser_apply(expr_in_brackets) }) %>%
    parser_or_else(function(){ consts }) %>%
    parser_or_else(function(){ double })
}

expr_in_brackets <- function(){
  between(symbol('('), symbol(')'), expr)
}

calculate <- function(s){
  expr() %>% parse(s)
}
