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
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
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

      dots_expressions <- rlang::quos(...)

      if (is.null(id)) {
        nested_alloms <- super$get_data('allometries') |>
          dplyr::filter(!!! dots_expressions) |>
          tidyr::nest(.by = allometry_id)
        res <- dplyr::pull(nested_alloms, data) |>
          purrr::map(.f = \(.x) {as.list(.x)}) |>
          purrr::set_names(nested_alloms[["allometry_id"]])
      } else {
        # argument validation (here, because is when first id is used)
        check_args_for(character = list(id = id))
        res <- super$get_data('allometries') |>
          dplyr::filter(allometry_id %in% id) |>
          tidyr::nest(.by = allometry_id) |>
          dplyr::pull(data) |>
          purrr::set_names(id) |>
          purrr::map(.f = \(.x) {as.list(.x)})
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
      check_args_for(character = list(allometry_id = allometry_id))
      silent_lapply <- lapply(
        dots_vars,
        function(x) {
          check_args_for(numeric = list(x = rlang::eval_tidy(x)))
        }
      )

      # allometry description
      allo_desc <- self$description(id = allometry_id)

      # variables in dots checks
      var_provided <- names(dots_vars)
      var_needed <- c(
        allo_desc[[allometry_id]][['independent_var_1']],
        allo_desc[[allometry_id]][['independent_var_2']],
        allo_desc[[allometry_id]][['independent_var_3']]
      ) |> magrittr::extract(!is.na(c(
        allo_desc[[allometry_id]][['independent_var_1']],
        allo_desc[[allometry_id]][['independent_var_2']],
        allo_desc[[allometry_id]][['independent_var_3']]
      )))

      var_needed_np <- var_needed[!var_needed %in% var_provided]
      if (length(var_needed_np) > 0) {
        stop(
          glue::glue(
            "variable(s) {glue::glue_collapse(var_needed_np, sep = ', ')}",
            " needed but not provided"
          )
        )
      }

      var_provided_nn <- var_provided[!var_provided %in% var_needed]
      if (length(var_provided_nn) > 0) {
        warning(
          glue::glue(
            "variable(s) {glue::glue_collapse(var_provided_nn, sep = ', ')} provided but",
            " not used"
          )
        )
      }


      # parameters from allometry (needed in equation)
      param_a <- allo_desc[[allometry_id]][['param_a']]
      param_b <- allo_desc[[allometry_id]][['param_b']]
      param_c <- allo_desc[[allometry_id]][['param_c']]
      param_d <- allo_desc[[allometry_id]][['param_d']]
      # equation parsing and evaluation
      preformatted_eqs <- allo_desc[[allometry_id]][['equation']] |>
        stringr::str_split(pattern = ' = ', n = 2) |>
        unlist() |>
        magrittr::extract(2) |>
        private$eq_formatter()

      # convert to exprs
      for (var in var_provided[var_provided %in% var_needed]) {
        preformatted_eqs <- stringr::str_replace_all(
          preformatted_eqs, pattern = var,
          replacement = paste0('rlang::eval_tidy(dots_vars$', var, ')')
        )
      }

      preformatted_eqs |>
        rlang::parse_expr() |>
        rlang::eval_tidy()
    },

    # describe method
    describe_var = function(variables) {

      # argument checking
      check_args_for(character = list(variables = variables))

      # cats
      allometries_describe_var_cat(
        variables, suppressMessages(self$get_data('thesaurus_app'))
      )

      # as the print method, this should return invisible(self) to allow $ piping
      return(invisible(self))

    }
  ),
  # private methods and values
  private = list(
    # connection values
    dbname = 'allometr_db',

    # equation formatter for using it to calculate
    eq_formatter = function(eq) {
      eq_res <- eq |>
        # this first step (remove '\u00C2') is in case no UTF-8 system, i.e. win 'latin1'
        stringr::str_remove_all('\u00C2') |>
        # replace · with *
        stringr::str_replace_all('\u00B7', '*') |>
        # replace \u00B2 and \u00B3 with \u005E2 and \u005E3
        stringr::str_replace_all('\u00B2', '\u005E2') |>
        stringr::str_replace_all('\u00B3', '\u005E3') |>
        # replace parameter letters with param_letter
        stringr::str_replace('\\ba\\b', 'param_a') |>
        stringr::str_replace('\\bb\\b', 'param_b') |>
        stringr::str_replace('\\bc\\b', 'param_c') |>
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
#' if (interactive()) {
#'   allomdb <- allometries()
#'   # tibble
#'   allometries_get_data(allomdb, 'allometries')
#'
#'   # we can use pipes
#'   allomdb |>
#'     allometries_get_data('allometries')
#'
#'   # allomdb is an R6 object, so the previous examples are the same as:
#'   allomdb$get_data('allometries')
#' }
#'
#' @export
allometries_get_data <- function(object, table_name = 'allometries') {
  # argument validation
  # NOTE: table_name is validated in the method
  check_class_for(object, 'lfcAllometries')
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
#' if (interactive()) {
#'   # by id
#'   allomdb <- allometries()
#'   foo <- allometries_description(allomdb, id = "GC_3210")
#'   foo
#'   foo$GC_3210$dependent_var
#'   foo$GC_3210$param_a
#'
#'   # filtering
#'   ht_dn_allometries <- allometries_description(allomdb, dependent_var %in% c("GC", "Dn"))
#'   length(ht_dn_allometries)
#'   names(ht_dn_allometries)
#'   ht_dn_allometries$GC_3210$dependent_var
#'   ht_dn_allometries$GC_3210$param_a
#' }
#'
#' @return A list with the selected allometries and their info, see examples. In the
#'   special case of no \code{id} or expressions (\code{...}) provided, information for
#'   all the allometries is returned.
#'
#' @export
allometries_description <- function(object, ..., id = NULL) {
  # argument validation
  check_class_for(object, 'lfcAllometries')
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
#' if (interactive()) {
#'   library(dplyr)
#'   allomdb <- allometries()
#'   allometries_calculate(allomdb, DR = c(0.55, 0.46, 0.37), allometry_id = "BH_287")
#'
#'   # inside a dplyr mutate, with a different allometry for each species
#'   iris_foo <- iris |>
#'     mutate(allom = rep(c("BH_287","BH_288","BH_290"), each = 50)) |>
#'     select(branch_diameter = Sepal.Length, Species, allom)
#'   iris_foo
#'
#'   iris_foo |>
#'     group_by(Species) |>
#'     mutate(BH = allometries_calculate(
#'         allomdb, DR = branch_diameter, allometry_id = first(allom)
#'     ))
#' }
#'
#' @export
allometries_calculate <- function(object, ..., allometry_id) {
  # argument validation
  check_class_for(object, 'lfcAllometries')
  # call to the class method
  object$calculate(..., allometry_id = allometry_id)
}

#' Print info about the variables present in the allometries db
#'
#' @description \code{allometries_describe_var} is a wrapper for the \code{$describe_var} method of
#'   \code{lfcAllometries} objects. See \code{\link{allometries}}.
#'
#' @param object \code{lfcAllometries} object, as created by \code{\link{allometries}}
#' @param variables character vector with the names of the variables to describe
#'
#' @return A character vector with the variable names to describe
#'
#' @family allometries functions
#'
#' @examples
#' if (interactive()) {
#' allometriesdb <- allometries()
#' allometries_describe_var(allometriesdb, "BR")
#' allometries_describe_var(allometriesdb, c("DBH", "P_BST"))
#'
#' # allometriesdb is an R6 object, so the previous example is the same as:
#' allometriesdb$describe_var("BR")
#' allometriesdb$describe_var(c("DBH", "P_BST"))
#' }
#'
#' @export
allometries_describe_var <- function(object, variables) {
  # argument validation
  check_class_for(object, 'lfcAllometries')
  # call to the class method
  object$describe_var(variables)
}
