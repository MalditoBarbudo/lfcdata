# syntactic sugar for "one or another if one is null" case. taken from tidyverse utils
`%||%` <- function(a, b) {
  if (rlang::is_null(a)) b else a
}

# syntactic sugar for "one or another if one is na" case.
`%na%` <- function(a, b) {
  if (rlang::is_na(a)) b else a
}

# argument checking
check_args_for <- function(
  character = NULL, numerical = NULL, logical = NULL, na = NULL
) {

  # browser()
  # characters
  if (!rlang::is_null(character)) {
    not_complying <- character %>%
      purrr::map(rlang::is_character) %>%
      purrr::keep(.p = ~!isTRUE(.x)) %>%
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not character\n"
      )
      stop(error_message)
    }
  }

  # numerical
  if (!rlang::is_null(numerical)) {
    not_complying <- numerical %>%
      purrr::map(rlang::is_bare_numeric) %>%
      purrr::keep(.p = ~!isTRUE(.x)) %>%
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "One or more variables are not numeric\n"
      )
      stop(error_message)
    }
  }

  # logical
  if (!rlang::is_null(logical)) {
    not_complying <- logical %>%
      purrr::map(rlang::is_logical) %>%
      purrr::keep(.p = ~!isTRUE(.x)) %>%
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not logical\n"
      )
      stop(error_message)
    }
  }

  # na
  if (!rlang::is_null(na)) {
    not_complying <- na %>%
      purrr::map(rlang::is_na) %>%
      purrr::keep(.p = ~isTRUE(.x)) %>%
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is missing\n"
      )
      stop(error_message)
    }
  }
}

# check for class
check_class_for <- function(object, class) {
  if (!inherits(object, class)) {
    error_message <- glue::glue("object is not from class {class}")
    stop(error_message)
  }
}

# check for length
check_length_for <- function(object, expected_length) {
  if (length(object) != expected_length) {
    error_message <- glue::glue("object must be of length {expected_length}")
    stop(error_message)
  }
}

# check if in a character vector
check_if_in_for <- function(object, in_template) {
  if (any(!object %in% in_template)) {
    error_message <- glue::glue("object must be one of {glue::glue_collapse(in_template, sep = ', ')}, not '{object}'")
    stop(error_message)
  }
}

# check if filter result is at least one row
check_filter_for <- function(object, error_message) {
  if (nrow(object) < 1) {
    stop(error_message)
  }
}
