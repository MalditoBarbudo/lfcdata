#' @description \code{allometries()} creates an object to access the allometries database.
#'
#' @title lfcAllometries class
#'
#' @return An \code{lfcAllometries} class object (inherits from \code{\link[R6]{R6Class}}),
#'   with methods to access the data. See Methods section.
#'
#' @section Methods:
#'   \code{lfcAllometries} objects has two public methods:
#'   \itemize{
#'     \item{\code{$get_data}: Retrieve and collect the allometries table. See
#'           \code{\link{allometries_get_data}} for more details}
#'     \item{\code{$description}: Retrieve the description for the desired
#'           allometries. See \code{\link{allometries_description}} for
#'           more details}
#'     \item{\code{$calculate}: Calculate variables based on the selected
#'           allometries. See \code{\link{allometries_calculate}} for
#'           more details}
#'     \item{\code{$describe_var}: Print the information available about the provided
#'           variable. See \code{\link{allometries_describe_var}} for more details}
#'   }
#'
#' @family Allometries functions
#'
#' @export
#'
#' @examples
#' allomdb <- allometries()
#' allomdb
allometries <- function() {
  lfcAllometries$new()
}

lfcAllometries <- R6::R6Class(
  # specs
  classname = 'lfcAllometries',
  inherit = lfcObject,
  cloneable = FALSE,
  # public methods and values
  public = list(
    # override default print
    print = function(...) {
      cat(
        " Access to the LFC allometries database.\n",
        crayon::blue$underline("laboratoriforestal.creaf.uab.cat\n\n"),
        "Use " %+% crayon::yellow$bold("allometries_get_data") %+%
          " to access the tables.\n",
        "Use " %+% crayon::yellow$bold("allometries_calculate") %+%
          " to calculate new values based on the allometries.\n",
        "Use " %+% crayon::yellow$bold("allometries_describe_var") %+%
          " to get the information available on the variables.\n",
        "See " %+%
          crayon::yellow$bold("vignette('tables_and_variables', package = 'lfcdata')") %+%
          " to learn more about the tables and variables."
      )
      invisible(self)
    },
    # allometries_descriptiom method. It returns the allometry as a list with all the
    # fields from the table. Is easy to use programmatically
    description = function(..., id = NULL) {

      # browser()
      dots_expressions <- rlang::quos(...)

      if (is.null(id)) {
        res <- super$get_data('allometries') %>%
          dplyr::filter(!!! dots_expressions) %>%
          split(.$allometry_id) %>%
          purrr::map(~ rlang::as_list(.x))
      } else {
        # argument validation (here, because is when first id is used)
        stopifnot(
          rlang::is_character(id)
        )
        res <- super$get_data('allometries') %>%
          dplyr::filter(allometry_id %in% id) %>%
          split(.$allometry_id) %>%
          purrr::map(~ rlang::as_list(.x))
      }

      if (length(res) < 1) {
        warning("No allometries were found. Returning an empty list")
      }
      return(res)
    },

    # allometries_calculate method
    calculate = function(..., allometry_id) {

      # variables
      dots_vars <- rlang::enquos(..., .named = FALSE)

      # argument validation
      stopifnot(
        rlang::is_character(allometry_id)
      )
      silent_lapply <- lapply(
        dots_vars,
        function(x) {
          stopifnot(is.numeric(rlang::eval_tidy(x)))
        }
      )

      # allometry description
      allo_desc <- self$description(id = allometry_id)
      # parameters from allometry (needed in equation)
      param_a <- allo_desc[[allometry_id]][['param_a']]
      param_b <- allo_desc[[allometry_id]][['param_b']]
      param_c <- allo_desc[[allometry_id]][['param_c']]
      param_d <- allo_desc[[allometry_id]][['param_d']]
      # equation parsing and evaluation
      allo_desc[[allometry_id]][['equation']] %>%
        stringr::str_split(pattern = ' = ', n = 2) %>%
        unlist() %>%
        magrittr::extract(2) %>%
        private$eq_formatter() %>% {
          for (var in names(dots_vars)) {
            # validate dots_vars are named
            stopifnot(stringr::str_length(var) > 0)
            . <- stringr::str_replace_all(
              ., pattern = var,
              replacement = paste0('rlang::eval_tidy(dots_vars$', var, ')')
            )
          }
          .
        } %>%
        rlang::parse_expr() %>%
        rlang::eval_tidy()
    },

    # describe method
    describe_var = function(variables) {

      # argument checking
      stopifnot(
        rlang::is_character(variables)
      )

      no_returned <- self$get_data('thesaurus_app') %>%
        dplyr::filter(text_id %in% variables) %>%
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
      invisible(self)

    }
  ),
  # private methods and values
  private = list(
    # connection values
    dbname = 'allometr_db',

    # equation formatter for using it to calculate
    eq_formatter = function(eq) {
      eq_res <- eq %>%
        stringr::str_replace_all('\u00B7', '*') %>%
        stringr::str_replace_all('\u00B2', '\u005E2') %>%
        stringr::str_replace_all('\u00B3', '\u005E3') %>%
        stringr::str_replace('\\ba\\b', 'param_a') %>%
        stringr::str_replace('\\bb\\b', 'param_b') %>%
        stringr::str_replace('\\bc\\b', 'param_c') %>%
        stringr::str_replace('\\bd\\b', 'param_d')

      return(eq_res)
    }
  )
)

#' Access to the tables in the allometries database
#'
#' @description \code{allometries_get_data} is a wrapper for the \code{$get_data} method of
#'   \code{lfcAllometries} objects. See also \code{\link{allometries}}.
#'
#' @param object \code{lfcAllometries} object, as created by \code{\link{allometries}}
#' @param table_name character vector of lenght 1 indicating the requested table name.
#'   Only value available at the moment is \code{"allometries"}
#'
#' @return A tbl object
#'
#' @family Allometries functions
#'
#' @details Connection to database can be slow. Tables retrieved from the db are stored
#'   in a temporary cache inside the lfcAllometries object created by
#'   \code{\link{allometries}}, making subsequent calls to the same table are faster.
#'
#' @examples
#' allomdb <- allometries()
#' # tibble
#' allometries_get_data(allomdb, 'allometries')
#'
#' # we can use pipes
#' allomdb %>%
#'   allometries_get_data('allometries')
#'
#' # allomdb is an R6 object, so the previous examples are the same as:
#' allomdb$get_data('allometries')
#'
#' @export
allometries_get_data <- function(object, table_name = 'allometries') {
  # argument validation
  # NOTE: table_name is validated in the method
  stopifnot(inherits(object, 'lfcAllometries'))
  # call to the class method
  object$get_data(table_name)
}

#' Info for allometries
#'
#' @description \code{allometries_description} is a wrapper for the \code{$description}
#'   method of \code{lfcAllometries} objects. See also \code{\link{allometries}}.
#'
#' @param object \code{lfcAllometries} object, as created by \code{\link{allometries}}
#' @param ... Expresions resolving to a logical value to filter the allometries. Only
#'   evaluated if \code{id} is NULL.
#' @param id Character vector with the allometry/ies id
#'
#' @family Allometries functions
#'
#' @examples
#'
#' # by id
#' allomdb <- allometries()
#' foo <- allometries_description(allomdb, id = "GC_2589")
#' foo$GC_2589$dependent_var
#' foo$GC_2589$param_a
#'
#' # filtering
#' ht_dn_allometries <- allometries_description(allomdb, dependent_var %in% c("GC", "Dn"))
#' ht_dn_allometries$GC_2589$dependent_var
#' ht_dn_allometries$GC_2589$param_a
#'
#' @return a list with the selected allometries and their info
#'
#' @export
allometries_description <- function(object, ..., id = NULL) {
  # argument validation
  stopifnot(inherits(object, 'lfcAllometries'))
  # call to the class method
  object$description(..., id = id)
}

#' Calculating new variables based on the allometries formula
#'
#' Return a vector with the desired allometry equation calculated
#'
#' @param object \code{lfcAllometries} object, as created by \code{\link{allometries}}
#' @param ... \bold{Must be named}. Numeric vectors for the independent variables
#'   present in the allometry equation. Each argument must be named as the
#'   independent variable the values correspond with. See examples.
#' @param allometry_id character with the unique allometry identifier
#'
#' @family Allometries functions
#'
#' @examples
#'
#' library(dplyr)
#' allomdb <- allometries()
#' allometries_calculate(allomdb, DR = c(0.55, 0.46, 0.37), allometry_id = "BH_287")
#'
#' # inside a dplyr mutate, with a different allometry for each species
#' iris_foo <- iris %>%
#'   mutate(allom = rep(c("BH_287","BH_288","BH_290"), each = 50)) %>%
#'   select(branch_diameter = Sepal.Length, Species, allom)
#' iris_foo
#'
#' iris_foo %>%
#'   group_by(Species) %>%
#'   mutate(BH = allometries_calculate(
#'       allomdb, DR = branch_diameter, allometry_id = first(allom)
#'   ))
#'
#' @export
allometries_calculate <- function(object, ..., allometry_id) {
  # argument validation
  stopifnot(inherits(object, 'lfcAllometries'))
  # call to the class method
  object$calculate(..., allometry_id = allometry_id)
}

#' Print info about the variables present in the allometries db
#'
#' @description \code{allometries_describe_var} is a wrapper for the \code{$describe_var} method of
#'   \code{lfcAllometries} objects. See \code{\link{allometries}}.
#'
#' @param object \code{lfcAllometries} object, as created by \code{\link{allometries}}
#'
#' @return A character vector with the variable names to describe
#'
#' @family allometries functions
#'
#' @examples
#' allometriesdb <- allometries()
#' allometries_describe_var(allometriesdb, "BR")
#' allometries_describe_var(allometriesdb, c("DBH", "P_BST"))
#'
#' # allometriesdb is an R6 object, so the previous example is the same as:
#' allometriesdb$describe_var("BR")
#' allometriesdb$describe_var(c("DBH", "P_BST"))
#'
#' @export
allometries_describe_var <- function(object, variables) {
  # argument validation
  stopifnot(inherits(object, 'lfcAllometries'))
  # call to the class method
  object$describe_var(variables)
}
