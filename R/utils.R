# syntactic sugar for "one or another if one is null" case. taken from tidyverse utils
`%||%` <- function(a, b) {
  if (rlang::is_null(a)) b else a
}
