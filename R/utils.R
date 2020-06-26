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
  character = NULL, numerical = NULL, logical = NULL, na = NULL,
  sf = NULL, points = NULL, polygons = NULL, date = NULL
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

  # sf
  if (!rlang::is_null(sf)) {
    not_complying <- sf %>%
      purrr::map(inherits, what = 'sf') %>%
      purrr::keep(.p = ~ !isTRUE(.x)) %>%
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not a simple feature (sf)\n"
      )
      stop(error_message)
    }
  }

  # points
  if (!rlang::is_null(points)) {
    not_complying <- points %>%
      purrr::map(
        ~ all(sf::st_is(.x, type = 'POINT'))
        # sf::st_is, type = 'POINT'
      ) %>%
      purrr::keep(.p = ~ !isTRUE(.x)) %>%
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not a POINT (sf)\n"
      )
      stop(error_message)
    }
  }

  # polygons
  if (!rlang::is_null(polygons)) {
    not_complying <- polygons %>%
      purrr::map(
        .f = function(x) {
          sf::st_is(x, 'POLYGON') | sf::st_is(x, 'MULTIPOLYGON')
        }
      ) %>%
      purrr::keep(.p = ~ !isTRUE(.x)) %>%
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} ",
        "is not a POLYGON or a MULTIPOLYGON (sf)\n"
      )
      stop(error_message)
    }
  }

  # dates
  if (!rlang::is_null(date)) {
    not_complying <- date %>%
      purrr::map(
        .f = function(x) {
          date_check <- try(as.Date(x))
          if (is(date_check, 'try-error')) {
            return(FALSE)
          }
          # if x is a vector, with valid and invalid dates as characters,
          # as.Date is going to return NAs for invalid ones, check for that too:
          if (any(is.na(date_check))) {
            return(FALSE)
          }
          return(TRUE)
        }
      ) %>%
      purrr::keep(.p = ~ !isTRUE(.x)) %>%
      names()

    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} ",
        "cannot be converted to date"
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
check_length_for <- function(object, expected_length, arg_name = 'object') {
  if (length(object) != expected_length) {
    error_message <- glue::glue("{arg_name} must be of length {expected_length}")
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

fes_table_dictionary <- function() {
  c(
    plot = 'Forest ecosystem service variables',

    comp = ' from the comparision between',

    nfi2 = ' NFI version 2 (1990-1991)',
    nfi3 = ' NFI version 3 (2000-2001)',
    nfi4 = ' NFI version 4 (2013-2016)',

    '2' = ' corresponding to the NFI version 2 (1990-1991)',
    '3' = ' corresponding to the NFI version 3 (2000-2001)',
    '4' = ' corresponding to the NFI version 4 (2013-2016)',

    variables = 'Thesaurus for variables',
    static = 'Static forest ecosystem service data'
  )
}

## cats functions (describe_* helpers) ####
nfi_describe_table_cat <- function(table, tables_dict, variables_thes) {

  # variables present in the table (courtesy of variables_thesaurus)
  variable_names <- variables_thes %>%
    dplyr::filter(.data$var_table == table) %>%
    dplyr::pull(.data$var_id) %>%
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

  # trick to use "." without CRAN note
  . <- NULL

  # get the var thes, the numerical var thes filter by the variable and
  # prepare the result with cat, glue and crayon, as a function to apply to
  # a vector of variables.
  variables_thes %>%
    dplyr::filter(.data$var_id == variable) %>% {
      check_filter_for(., glue::glue("{variable} variable not found"))
      .
    } %>%
    dplyr::left_join(numerical_thes, by = c("var_id", "var_table")) %>%
    dplyr::group_by(.data$var_description_eng) %>%
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

  # trick to use "." withou CRAN note
  . <- NULL

  no_returned <- thes %>%
    dplyr::filter(.data$text_id %in% variables) %>% {
      check_filter_for(., glue::glue("one or more variables not found"))
      .
    } %>%
    dplyr::group_by(.data$translation_eng) %>%
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
  return(invisible(NULL))
}

lidar_describe_var_cat <- function(variables, thes) {
  no_returned <- thes %>%
    dplyr::filter(.data$var_id %in% variables) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$var_id) %>%
    dplyr::group_walk(
      ~ cat(
        # var name
        crayon::yellow$bold(.x$translation_eng),
        "\n",
        # var units
        "Units:  ",
        crayon::blue$bold("[") %+%
          crayon::blue$italic$bold(
            glue::glue("{(.x$var_units %na% ' - ') %>% unique()}")
          ) %+%
          crayon::blue$bold("]"),
        "\n",
        "Details:  ",
        crayon::blue$italic$bold(
          glue::glue("{(.x$var_description_eng %na% ' - ') %>% unique()}")
        ),
        "\n\n",
        sep = ''
      )
    )
  return(invisible(NULL))
}

fes_describe_table_cat <- function(table, tables_dict, variables_thes) {

  # variables present in the table (courtesy of variables_thesaurus)
  variable_names <- variables_thes %>%
    dplyr::filter(.data$var_table == table) %>%
    dplyr::pull(.data$var_id) %>%
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

fes_describe_var_cat <- function(variable, variables_thes) {

  # trick to use "." without CRAN note
  . <- NULL

  # get the var thes, the numerical var thes filter by the variable and
  # prepare the result with cat, glue and crayon, as a function to apply to
  # a vector of variables.
  variables_thes %>%
    dplyr::filter(.data$var_id == variable) %>% {
      check_filter_for(., glue::glue("{variable} variable not found"))
      .
    } %>%
    dplyr::group_by(.data$var_description_eng) %>%
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

# Combine statistics for different groups. ST_SummaryStats in postgis SQL returns the
# statistics for each tile the polygon intersects. This means that calcualting the n and
# the mean is easy, but we need to combine the statistics to calculate the standard
# deviation of the entire sample.
# We use the Cochrane Reviews formulae:
# https://handbook-5-1.cochrane.org/front_page.htm
# https://handbook-5-1.cochrane.org/chapter_7/table_7_7_a_formulae_for_combining_groups.htm
# cochrane_sd_reduce <- function(n, m, s) {
#
#   res_vec <- seq_along(n) %>%
#     # we need to work with vectors of c(n1,m1,s1); c(n2,m2,s2)...
#     purrr::map(~ c(n = n[.x], m = m[.x], s = s[.x])) %>%
#     # and we reduce those vectors to create a new vector with the groups combined stats
#     # to use with the following vector:
#     #
#     # at start:         list(c(n1,m1,s1), c(n2,m2,s2), ..., c(nn,mn,sn))
#     # after first step: list(c(nc1-2,mc1-2,sc1-2), ..., c(nn,mn,sn))
#     # after ... steps:  list(c(nc1-...,mc1-...,sc1-...), c(nn,mn,sn))
#     # after last step:  list(c(nc1-n,mc1-n,sc1-n))
#     purrr::reduce(
#       .f = function(first, second) {
#         stddev <- sqrt(
#           # numerador
#           (((first['n'] - 1)*(first['s']^2)) + ((second['n'] - 1)*(second['s']^2)) +
#              (((first['n']*second['n'])/(first['n'] + second['n']))*(first['m']^2 + second['m']^2 - (2*first['m']*second['m'])))) /
#             # denominador
#             (first['n'] + second['n'] - 1)
#         ) %>% unname()
#         count <- sum(first['n'], second['n'])
#         mean_temp <- (((first['n']*first['m']) + (second['n']*second['m']))/count) %>% unname()
#
#         return(c(n = count, m = mean_temp, s = stddev))
#       }
#     )
#   # we have the final vector and we retrieve sc1-n (the combined standard deviation)
#   return(res_vec['s'])
# }
