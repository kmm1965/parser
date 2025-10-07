library(maybe)

as_parser <- function(.f) {
  structure(.f, class = "parser")
}

is_parser <- function(.p) {
  identical(class(.p), "parser")
}

assert_is_parser <- function(p) {
  if (is_parser(p))
    p
  else {
    print(class(p))
    stop(
      "The argument to a function must be a parser.",
      call. = FALSE
    )
  }
}

parse <- function(.p, inp) {
  (.p %>% assert_is_parser())(inp)
}

parser_map <- function(.p, .f){
  .p %>% assert_is_parser()
  as_parser(function(inp) { .p %>% parse(inp) %>% fmap(function (pair) { list(.f(pair[[1]]), pair[[2]]) }) })
}

parser_and_then <- function(.p, .f){
  .p %>% assert_is_parser()
  as_parser(function(inp) { .p %>% parse(inp) %>% and_then(function (pair) { .f(pair[[1]]) %>% parse(pair[[2]])  }) })
}

parser_skip <- function(.p, .fp){
  .p %>% parser_and_then(function(x) { .fp() })
}

parser_apply <- function(.pf, .fp){
  .pf %>% parser_and_then(function(f) { .fp() %>% parser_map(f) })
}

parser_or_else <- function(.p, .fp){
  .p %>% assert_is_parser()
  as_parser(function(inp) {
    m <- .p %>% parse(inp)
    if(is_just(m)) m else .fp() %>% parse(inp)
  })
}

pure <- function(x) {
  as_parser(function(inp){ just(list(x, inp)) })
}

empty <- as_parser(function(inp){ nothing() })
