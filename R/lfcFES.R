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
nfi <- function() {
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
            # if it is, use the sf read to get the spatial one
            query_data_spatial <- sf::st_read(
              private$pool_conn, table_name
            )
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
        sort()
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
        crayon::blue$underline("laboratoriforestal.creaf.uab.cat\n\n"),
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
