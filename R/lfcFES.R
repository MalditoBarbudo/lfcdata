#' @description \code{fes()} creates an object to access the fes database.
#'
#' @title lfcFES class
#'
#' @return An \code{lfcFES} class object (inherits from
#'   \code{\link[R6]{R6Class}}), with methods to access the data. See Methods
#'   section.
#'
#' @section Methods:
#'   \code{lfcFES} objects has the following public methods:
#'   \itemize{
#'     \item{\code{$get_data}: Retrieve and collect FES database tables. See
#'           \code{\link{fes_get_data}} for more details}
#'     \item{\code{$avail_tables}: Return a character vector with the names of
#'           the available tables in the database. See
#'           \code{\link{fes_avail_tables}} for more details}
#'     \item{\code{$describe_table}: Print the information available about the
#'           provided table. See \code{\link{fes_describe_table}} for more
#'           details}
#'     \item{\code{$describe_var}: Print the information available about the
#'           provided variable. See \code{\link{fes_describe_var}} for more
#'           details}
#'   }
#'
#' @family FES functions
#'
#' @export
#'
#' @examples
#' fesdb <- fes()
#' fesdb
fes <- function() {
  lfcFES$new()
}

#' @importFrom R6 R6Class
#' @importFrom crayon %+%
#' @importFrom rlang .data
lfcFES <- R6::R6Class(
  # specs
  classname = "lfcFES",
  inherit = lfcObject,
  cloneable = FALSE,
  # public methods and values
  public = list(
    # get method, modifying the super class method
    get_data = function(table_name, spatial = FALSE) {

      # arguments validation (table name is always validated in the super)
      check_args_for(
        logical = list(spatial = spatial),
        na = list(spatial = spatial)
      )

      res <- private$data_cache[[
        glue::glue("{table_name}_{as.character(spatial)}")
      ]] %||%
        {
          # is the query spatial?
          if (!spatial) {
            # if not, return the data as is
            # here we dont update cache, because is done in the super method
            super$get_data(table_name)
          } else {
            message('Querying table from LFC database, this can take a while...')
            # if it is, use the sf read to get the spatial one
            query_data_spatial <- sf::st_read(
              private$pool_conn, table_name
            )
            message('Done')
            # update cache
            private$data_cache[[
              glue::glue("{table_name}_{as.character(spatial)}")
            ]] <- query_data_spatial
            query_data_spatial
          }
        }

      return(res)
    },

    # available tables method
    avail_tables = function() {
      dplyr::db_list_tables(private$pool_conn) %>%
        tolower() %>%
        unique() %>%
        sort() %>%
        magrittr::extract(stringr::str_detect(., 'plot|static|thesaurus'))
    },

    # describe table method
    describe_table = function(tables) {
      # argument checking
      check_args_for(character = list(tables = tables))
      check_if_in_for(tables, self$avail_tables())

      # table name dictionary and variables thesaurus
      tables_dict <- fes_table_dictionary()
      variables_thes <- suppressMessages(self$get_data('variables_thesaurus'))

      # map to apply to all tables
      tables %>%
        purrr::map(
          fes_describe_table_cat,
          tables_dict = tables_dict, variables_thes = variables_thes
        )

      # as the print method, this should return invisible(self) to allow $ piping
      return(invisible(self))
    },

    # describe variable method
    describe_var = function(variables) {

      # argument checking
      check_args_for(character = list(variables = variables))

      # numerical and variables thesauruses
      variables_thes <- suppressMessages(self$get_data('variables_thesaurus'))

      # map to apply to all variables
      variables %>%
        purrr::map(
          fes_describe_var_cat,
          variables_thes = variables_thes
        )

      # as the print method, this should return invisible(self) to allow $ piping
      invisible(self)

    },

    # override default print
    print = function(...) {
      cat(
        " Access to the Forest Ecosystem Services data for Catalonia.\n",
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
        "Use " %+% crayon::yellow$bold("fes_get_data") %+%
          " to access the tables.\n",
        "Use " %+% crayon::yellow$bold("fes_avail_tables") %+%
          " to know which tables are available.\n",
        "Use " %+% crayon::yellow$bold("fes_describe_table") %+%
          " to get information about an specific table.\n",
        "Use " %+% crayon::yellow$bold("fes_describe_var") %+%
          " to get the information available on the variables.\n",
        "See " %+%
          crayon::yellow$bold(
            "vignette('tables_and_variables', package = 'lfcdata')"
          ) %+%
          " to learn more about the tables and variables."
      )
      invisible(self)
    }
  ),
  # private methods and values
  private = list(
    # connection values
    dbname = 'forestecoserv'
  )
)

#' Access to the tables in the FES database
#'
#' @description \code{fes_get_data} is a wrapper for the \code{$get_data} method
#'   of \code{lfcFES} objects. See also \code{\link{fes}}.
#'
#' @param object \code{lfcFES} object, as created by \code{\link{fes}}
#' @param table_name character vector of lenght 1 indicating the requested table
#'   name
#' @param spatial logical indicating if the data must be converted to an spatial
#'   object
#'
#' @return A tbl object: tbl_df if spatial is \code{FALSE}, sf if spatial is
#'   \code{TRUE}
#'
#' @family FES functions
#'
#' @details Connection to database can be slow. Tables retrieved from the db are
#'   stored in a temporary cache inside the lfcFES object created by
#'   \code{\link{fes}}, making subsequent calls to the same table are faster.
#'
#' @examples
#' if (interactive()) {
#'   fesdb <- fes()
#'   # tibble
#'   fes_get_data(fesdb, 'static')
#'   # sf tibble
#'   fes_get_data(fesdb, 'static', TRUE)
#'
#'   # we can use pipes
#'   fesdb %>%
#'     fes_get_data('static', TRUE)
#'
#'   # fesdb is an R6 object, so the previous examples are the same as:
#'   fesdb$get_data('static')
#'   fesdb$get_data('static', TRUE)
#' }
#'
#' @export
fes_get_data <- function(object, table_name, spatial = FALSE) {
  # argument validation
  # NOTE: table_name and spatial are validated in the method
  check_class_for(object, 'lfcFES')
  # call to the class method
  object$get_data(table_name, spatial)
}

#' Get the available tables in FES db
#'
#' @description \code{fes_avail_tables} is a wrapper for the \code{$avail_tables}
#'   method of \code{lfcFES} objects. See \code{\link{fes}}.
#'
#' @param object \code{lfcFES} object, as created by \code{\link{fes}}
#'
#' @return A character vector with the table names
#'
#' @family FES functions
#'
#' @examples
#' if (interactive()) {
#'   fesdb <- fes()
#'   fes_avail_tables(fesdb)
#'
#'   # fesdb is an R6 object, so the previous example is the same as:
#'   fesdb$avail_tables()
#' }
#'
#' @export
fes_avail_tables <- function(object) {
  # argument validation
  check_class_for(object, 'lfcFES')
  # call to the class method
  object$avail_tables()
}

#' Print info about the variables present in the FES db
#'
#' @description \code{fes_describe_var} is a wrapper for the \code{$describe_var}
#'   method of \code{lfcFES} objects. See \code{\link{fes}}.
#'
#' @param object \code{lfcFES} object, as created by \code{\link{fes}}
#' @param variables character vector with the names of the variables to describe
#'
#' @return Description is printed in the console, nothing is returned
#'
#' @family FES functions
#'
#' @examples
#' if (interactive()) {
#'   fesdb <- fes()
#'   fes_describe_var(fesdb, "mushrooms_poduction")
#'   fes_describe_var(fesdb, c("exported_water", "animals_presence"))
#'
#'   # fesdb is an R6 object, so the previous example is the same as:
#'   fesdb$describe_var("mushrooms_poduction")
#'   fesdb$describe_var(c("exported_water", "animals_presence"))
#' }
#'
#' @export
fes_describe_var <- function(object, variables) {
  # argument validation
  check_class_for(object, 'lfcFES')
  # call to the class method
  object$describe_var(variables)
}

#' Print info about the tables present in the FES db
#'
#' @description \code{fes_describe_table} is a wrapper for the \code{$describe_table}
#'   method of \code{lfcFES} objects. See \code{\link{fes}}.
#'
#' @param object \code{lfcFES} object, as created by \code{\link{fes}}
#' @param tables character vector with the names of the tables to describe
#'
#' @return Description is printed in the console, nothing is returned
#'
#' @family fes functions
#'
#' @examples
#' if (interactive()) {
#'   fesdb <- fes()
#'   fes_describe_table(fesdb, "static")
#'   fes_describe_table(fesdb, c("static", "plot_nfi_2_results"))
#'
#'   # fesdb is an R6 object, so the previous example is the same as:
#'   fesdb$describe_table("static")
#'   fesdb$describe_table(c("static", "plot_nfi_2_results"))
#' }
#'
#' @export
fes_describe_table <- function(object, tables) {
  # argument validation
  check_class_for(object, 'lfcFES')
  # call to the class method
  object$describe_table(tables)
}
