#' @description \code{nfi()} creates an object to access the nfi database.
#'
#' @title lfcNFI class
#'
#' @return An \code{lfcNFI} class object (inherits from \code{\link[R6]{R6Class}}),
#'   with methods to access the data. See Methods section.
#'
#' @section Methods:
#'   \code{lfcNFI} objects has two public methods:
#'   \itemize{
#'     \item{\code{$get_data}: Retrieve and collect NFI database tables. See
#'           \code{\link{nfi_get_data}} for more details}
#'     \item{\code{$avail_tables}: Return a character vector with the names of the
#'           available tables in the database. See \code{\link{nfi_avail_tables}} for
#'           more details}
#'   }
#'
#' @family NFI functions
#'
#' @export
#'
#' @examples
#' nfidb <- nfi()
#' nfidb
nfi <- function() {
  lfcNFI$new()
}

#' importFrom R6 R6Class
lfcNFI <- R6::R6Class(
  # specs
  classname = "lfcNFI",
  cloneable = FALSE,
  # public methods and values
  public = list(
    # initialize method
    initialize = function() {
      private$pool_conn <- private$pool_conn_create()
    },

    # get method, accepting a table name
    get_data = function(table_name, spatial = FALSE) {

      # arguments validation
      stopifnot(
        rlang::is_character(table_name) & length(table_name) == 1,
        rlang::is_logical(spatial) & !rlang::is_na(spatial)
      )

      # return the cached data if exists. If no cache, retrieve the data from db
      # and update the cache
      # NOTE: %||% is in utils.R, simplifies the syntax and the readibility of the
      # expression.
      res <- private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]] %||%
        {
          temp_res <- lfcdata:::.lfcproto_get_data(private, table_name, spatial)
          private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]] <- temp_res
          temp_res
        }

      return(res)
    },

    # available tables method
    avail_tables = function() {
      dplyr::db_list_tables(private$pool_conn) %>%
        tolower() %>%
        unique()
    },

    # override default print
    print = function(...) {
      cat(
        " Access to the Spanish National Forest Inventory data for Catalonia.\n",
        "(laboratoriforestal.creaf.uab.cat)\n\n",
        "Use nfi_get_data to access the tables.\n",
        "Use nfi_avail_tables to know which tables are available.\n",
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
    dbname = 'tururu',

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

#' Access to the tables in the NFI database
#'
#' @description \code{nfi_get_data} is a wrapper for the \code{$get_data} method of
#'   \code{lfcNFI} objects. See \code{\link{nfi}}.
#'
#' @param object \code{lfcNFI} object, as created by \code{\link{nfi}}
#' @param table_name character vector of lenght 1 indicating the requested table name
#' @param spatial logical indicating if the data must be converted to an spatial object
#'
#' @return A tbl object: tbl_df if spatial is \code{FALSE}, sf if
#'   spatial is \code{TRUE}
#'
#' @family NFI functions
#'
#' @details Connection to database can be slow. Tables retrieved from the db are stored
#'   in a temporary cache inside the lfcNFI object created by \code{\link{nfi}}, making
#'   subsequent calls to the same table are faster.
#'
#' @examples
#' nfidb <- nfi()
#' # tibble
#' nfi_get_data(nfidb, 'plots')
#' # sf tibble
#' nfi_get_data(nfidb, 'plots', TRUE)
#'
#' # we can use pipes
#' library(dplyr)
#' nfidb %>%
#'   nfi_get_data('plots', TRUE)
#'
#' # nfidb is an R6 object, so the previous examples are the same as:
#' nfidb$get_data('plots')
#' nfidb$get_data('plots', TRUE)
#'
#' @export
nfi_get_data <- function(object, table_name, spatial = FALSE) {
  # argument validation
  # NOTE: table_name and spatial are validated in the method
  stopifnot(inherits(object, 'lfcNFI'))
  # call to the class method
  object$get_data(table_name, spatial)
}

#' Get the available tables in NFI db
#'
#' @description \code{nfi_avail_tables} is a wrapper for the \code{$avail_tables} method of
#'   \code{lfcNFI} objects. See \code{\link{nfi}}.
#'
#' @param object \code{lfcNFI} object, as created by \code{\link{nfi}}
#'
#' @return A character vector with the table names
#'
#' @family NFI functions
#'
#' @examples
#' nfidb <- nfi()
#' nfi_avail_tables(nfidb)
#'
#' # nfidb is an R6 object, so the previous example is the same as:
#' nfidb$avail_tables()
#'
#' @export
nfi_avail_tables <- function(object) {
  # argument validation
  stopifnot(inherits(object, 'lfcNFI'))
  # call to the class method
  object$avail_tables()
}
