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

defObject <- function(n, value){
  name(n) %>% parser_skip(function(){ pure(value) })
}

funcs <- fold(list(
  defObject("sin",   sin),
  defObject("cos",   cos),
  defObject("asin",  asin),
  defObject("acos",  acos),
  defObject("sinh",  sinh),
  defObject("cosh",  cosh),
  defObject("asinh", asinh),
  defObject("acosh", acosh),
  defObject("tan",   tan),
  defObject("log",   log),
  defObject("log10", log10),
  defObject("exp",   exp),
  defObject("sqrt",  sqrt),
  defObject("sqr",   function(x){ x * x })
))

consts <- fold(list(
  defObject("E",        2.7182818284590452),
  defObject("PI",       pi),
  defObject("LOG2E",    1.44269504088896340736),  # log2(e)
  defObject("LOG10E",   0.434294481903251827651), # log10(e)
  defObject("LN2",      0.693147180559945309417), # ln(2)
  defObject("LN10",     2.30258509299404568402),  # ln(10)
  defObject("PI_2",     1.57079632679489661923),  # pi/2
  defObject("PI_4",     0.785398163397448309616), # pi/4
  defObject("1_PI",     0.318309886183790671538), # 1/pi
  defObject("2_PI",     0.636619772367581343076), # 2/pi
  defObject("2_SQRTPI", 1.12837916709551257390),  # 2/sqrt(pi)
  defObject("SQRT2",    1.41421356237309504880),  # sqrt(2)
  defObject("SQRT1_2",  0.707106781186547524401)  # 1/sqrt(2)
))

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
