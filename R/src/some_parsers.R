source("../src/parser.R")

empty_string <- pure("")

some <- function(p) {
  p %>% parser_map(function(c) { function(s) { paste0(c, s) }}) %>% parser_apply( function(){ many(p) } )
}

many <- function(p) {
  some(p) %>% parser_or_else(function() { empty_string })
}

any_char <- as_parser(function(inp) {
  if(nchar(inp) > 0)
    just(list(substring(inp, 1, 1), substring(inp, 2)))
  else nothing()
})

satisfy <- function(pred) {
  any_char %>% parser_and_then(function(c) { if(pred(c)) pure(c) else empty })
}

char <- function(c) {
  satisfy(function(x){ x == c })
}

spaces <- many(satisfy(function(c){ grepl("\\s", c) }))
  
between <- function(open, close, fp){
  open %>% parser_skip(function(){ fp() %>% parser_and_then(function(x){ close %>% parser_skip(function(){ pure(x) }) }) })
}

token <- function(p) {
  between(spaces, spaces, function(){ p })
}

symbol <- function(c) {
  char(c) %>% token()
}

alnum <- satisfy(function(c){ grepl("[[:alnum:]]", c) || c == '_' })

name <- function(n) {
  some(alnum) %>% parser_and_then(function(n2){ if(n2 == n) pure(n) else empty }) %>% token()
}

optional_s <- function(p) {
  p %>% parser_or_else(function(){ empty_string })
}

digit = satisfy( function(c){ grepl("\\d", c) })

digits <- many(digit)

sign <- optional_s(char('+') %>% parser_or_else(function(){ char('-') }))

usign <- optional_s(symbol('+') %>% parser_or_else(function(){ symbol('-') }))

double <- token(digits %>%
  parser_and_then(function(int_part) { optional_s(char('.') %>% parser_skip(function() { digits })) %>%
  parser_and_then(function(frac_part) { optional_s(char('e') %>% parser_or_else(function() { char('E') }) %>% parser_skip(function(){ sign }) %>%
    parser_and_then(function(exp_sign) { some(digit) %>%
    parser_and_then(function(exp_digits) { pure(paste0(exp_sign, exp_digits)) }) })) %>%
  parser_and_then(function(exp_part) { if(nchar(int_part) > 0 || nchar(frac_part) > 0)
   pure(as.numeric(paste0(int_part,
     if(nchar(frac_part) > 0) paste0('.', frac_part) else "",
     if(nchar(exp_part) > 0) paste0('e', exp_part) else "")))
   else empty }) }) }))

rest <- function(fp, ff, op, x) {
  op %>% parser_and_then(function(f) {
    fp() %>% parser_and_then(function(y) { ff(f(x, y)) })
  }) %>% parser_or_else(function(){ pure(x) })
}

rest_l <- function(p, op, x) {
  rest(function(){ p }, function(y){ rest_l(p, op, y) }, op, x)
}

chainl1 <- function(p, op, negate_first) {
  p %>% parser_and_then(function(x) { rest_l(p, op, if(negate_first) -x else x) })
}

rest_r <- function(p, op, x) {
  rest(function(){ chainr1(p, op) }, function(y){ pure(y) }, op, x)
}

chainr1 <- function(p, op) {
  p %>% parser_and_then(function(x) { rest_r(p, op, x) })
}
