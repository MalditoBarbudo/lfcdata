## syntactic sugars ####
# syntactic sugar for "one or another if one is null" case. taken from tidyverse utils
`%||%` <- function(a, b) {
  if (rlang::is_null(a)) b else a
}

# syntactic sugar for "one or another if one is na" case.
`%na%` <- function(a, b) {
  if (rlang::is_na(a)) b else a
}

## argument and other checks ####
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
    not_complying <- which(!object %in% in_template)
    allowed_entries <- glue::glue_collapse(in_template, sep = ', ', width = 120)
    error_message <- glue::glue(
      "{object[not_complying]} not found. Must be one of {allowed_entries}"
      # "object must be one of {allowed_entries}, not '{object}'"
    )
    stop(error_message)
  }
}

# check if filter result is at least one row
check_filter_for <- function(object, error_message) {
  if (nrow(object) < 1) {
    stop(error_message)
  }
}

## dictionaries and lookup tables ####
nfi_table_dictionary <- function() {
  c(
    plot = 'Plots eco-physiological variables',
    species = 'Plots eco-physiological variables broken down by species',
    simpspecies = 'Plots eco-physiological variables broken down by simplified species',
    genus = 'Plots eco-physiological variables broken down by genus',
    dec = 'Plots eco-physiological variables broken down by deciduous/esclerophyill/conifers',
    bc = 'Plots eco-physiological variables broken down by broadleaf and conifers',

    diamclass = ', also broken down by diameter classes',

    comp = ' from the comparision between',

    nfi2 = ' NFI version 2 (1990-1991)',
    nfi3 = ' NFI version 3 (2000-2001)',
    nfi4 = ' NFI version 4 (2013-2016)',

    '2' = ' from NFI version 2 (1990-1991)',
    '3' = ' from NFI version 3 (2000-2001)',
    '4' = ' from NFI version 4 (2013-2016)',

    plots = 'Plots general info',
    dynamic = 'dynamic (meaning it changes for each NFI version)',

    shrub = 'Plots eco-physiological variables broken down by shrub species',
    regeneration = 'Plots eco-physiological variables for small trees broken down by species',

    variables = 'Thesaurus for variables',
    numerical = ' (limits and units for numerical variables)'
  )
}

## cats functions (describe_* helpers) ####
nfi_describe_table_cat <- function(table, tables_dict, variables_thes) {

  # variables present in the table (courtesy of variables_thesaurus)
  variable_names <- variables_thes %>%
    dplyr::filter(var_table == table) %>%
    dplyr::pull(var_id) %>%
    unique()

  # table name deconstructed to query the nfi table dictionary
  table_deconstructed <- stringr::str_split(table, '_') %>%
    purrr::flatten_chr()

  ## cats
  # table name
  cat('\n', crayon::yellow$bold(table), '\n', sep = '')
  # table description
  cat(
    glue::glue("{tables_dict[table_deconstructed] %>% purrr::discard(is.na)}") %>%
      glue::glue_collapse() %>%
      crayon::green() %>%
      strwrap(width = 75),
    # '\n',
    fill = 80, sep = ''
  )
  # table variables
  cat('Variables in table:\n')
  cat(
    glue::glue(" - {sort(variable_names)}") %>%
      glue::glue_collapse(sep = '\n') %>%
      crayon::magenta()
  )
  cat('\n')

  # return nothing (invisible NULL)
  return(invisible(NULL))
}

nfi_describe_var_cat <- function(variable, variables_thes, numerical_thes) {
  # get the var thes, the numerical var thes filter by the variable and
  # prepare the result with cat, glue and crayon, as a function to apply to
  # a vector of variables.
  variables_thes %>%
    dplyr::filter(var_id == variable) %>% {
      check_filter_for(., glue::glue("{variable} variable not found"))
      .
    } %>%
    dplyr::left_join(numerical_thes, by = c("var_id", "var_table")) %>%
    dplyr::group_by(var_description_eng) %>%
    dplyr::group_walk(
      ~ cat(
        "\n",
        # var name
        crayon::yellow$bold(glue::glue(
          "{.x$translation_eng %>% unique()} ({.x$var_id %>% unique()})"
        )),
        "\n",
        # var description
        strwrap(crayon::green(.y$var_description_eng), width = 72),
        "\n",
        # var units
        crayon::blue$bold(
          "Units: [" %+%
            crayon::blue$italic$bold(
              glue::glue("{(.x$var_units %na% ' - ') %>% unique()}")
            ) %+%
            "]"
        ),
        "\n",
        # tables present
        "Present in the following tables:\n",
        crayon::magenta(glue::glue_collapse(
          glue::glue(" - {sort(.x$var_table)}"), sep = '\n'
        )),
        # cat options
        sep = '', fill = 80
      )
    )
  # return nothing (invisible NULL)
  return(invisible(NULL))
}

allometries_describe_var_cat <- function(variables, thes) {
  no_returned <- thes %>%
    dplyr::filter(text_id %in% variables) %>% {
      check_filter_for(., glue::glue("one or more variables not found"))
      .
    } %>%
    dplyr::group_by(translation_eng) %>%
    dplyr::group_walk(
      ~ cat(
        # var name
        crayon::yellow$bold(stringr::str_split_fixed(.y$translation_eng, ' \\(', 2)[1]),
        "\n",
        # var units
        "Units:  ",
        crayon::blue$bold("[") %+%
          crayon::blue$italic$bold(
            glue::glue("{(.x$var_units %na% ' - ') %>% unique()}")
          ) %+%
          crayon::blue$bold("]"),
        "\n",
        "English abbreviation:  ",
        crayon::blue$italic$bold(
          glue::glue("{(.x$var_abbr_eng %na% ' - ') %>% unique()}")
        ),
        "\n",
        "Data abbreviation:  ",
        crayon::blue$italic$bold(
          glue::glue("{(.x$var_abbr_spa %na% ' - ') %>% unique()}")
        ),
        "\n\n",
        sep = ''
      )
    )
}
