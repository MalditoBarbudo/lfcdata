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
#'           \code{\link{allom_get_data}} for more details}
#'     \item{\code{$???}: xxxx. See \code{\link{allom_???}} for
#'           more details}
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
  cloneable = FALSE,
  # public methods and values
  public = list(
    # initialize method
    initialize = function() {
      private$pool_conn <- private$pool_conn_create()
    },

    # get method, accepting a table name
    get_data = function(table_name) {

      # arguments validation
      stopifnot(
        rlang::is_character(table_name) & length(table_name) == 1
      )

      # return the cached data if exists. If no cache, retrieve the data from db
      # and update the cache
      # NOTE: %||% is in utils.R, simplifies the syntax and the readibility of the
      # expression.
      res <- private$data_cache[[table_name]] %||%
        {
          temp_res <- lfcdata:::.lfcproto_get_data(private, table_name, spatial = FALSE)
          private$data_cache[[table_name]] <- temp_res
          temp_res
        }

      return(res)
    },

    # override default print
    print = function(...) {
      cat(
        " Access to the LFC allometries database.\n",
        "(laboratoriforestal.creaf.uab.cat)\n\n",
        "Use allom_get_data to access the tables.\n",
        "See vignette('tables_and_variables', package = 'lfcdata') to learn more about the tables and variables."
      )
      invisible(self)
    }
  ),
  # private methods and values
  private = list(
    # connection values
    host = 'laboratoriforestal.creaf.uab.cat',
    port = 5432,
    user = 'guest',
    pass = 'guest',
    dbname = 'allometr_db',

    # cache object
    data_cache = list(),

    # pool connection
    pool_conn = NULL,

    # finalize method
    finalize = function() {
      # when object is collected or R session exits, close the db connections
      pool::poolClose(private$pool_conn)
    },

    # initialize method function
    pool_conn_create = function() {
      pool::dbPool(
        drv = RPostgreSQL::PostgreSQL(),
        user = private$user,
        password = private$pass,
        dbname = private$dbname,
        host = private$host,
        port = private$port,
        idleTimeout = 3600000
      )
    }
  )
)

#' Access to the tables in the allometries database
#'
#' @description \code{allometries_get_data} is a wrapper for the \code{$get_data} method of
#'   \code{lfcAllometries} objects. See \code{\link{allometries}}.
#'
#' @param object \code{lfcAllometries} object, as created by \code{\link{allometries}}
#' @param table_name character vector of lenght 1 indicating the requested table name
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
#' allomdb <- nfi()
#' # tibble
#' allometries_get_data(allomdb, 'allometries')
#'
#' # we can use pipes
#' library(dplyr)
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
