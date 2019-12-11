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
        "(laboratoriforestal.creaf.uab.cat)\n",
        "See ?nfi_get_data to know how to access the tables"
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

.lfcproto_get_data <- function(private, table_name, spatial) {

  # try to catch a db connection error
  query_data <- try(
    dplyr::tbl(private$pool_conn, table_name) %>% dplyr::collect()
  )
  if (inherits(query_data, "try-error")) {
    stop("Can not connect to the database:\n", query_data[1])
  }

  # is the query spatial?
  if (!spatial) {
    # if not,return the data
    return(query_data)
  } else {
    # if it is, then convert based on the lat and long vars
    if (all(c('coords_longitude', 'coords_latitude') %in% names(query_data))) {
      query_data_spatial <- query_data %>%
        sf::st_as_sf(
          coords = c('coords_longitude', 'coords_latitude'), remove = FALSE,
          crs = 4326
        )
    } else {
      # if there is no lat long vars, then get them from plots
      query_data_spatial <- query_data %>%
        dplyr::left_join(
          dplyr::tbl(private$pool_conn, 'plots') %>%
            dplyr::select(plot_id, coords_longitude, coords_latitude) %>%
            dplyr::collect(),
          by = 'plot_id'
        ) %>%
        sf::st_as_sf(
          coords = c('coords_longitude', 'coords_latitude'), remove = FALSE,
          crs = 4326
        )
    }
    return(query_data_spatial)
  }

}

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
#'   subsequent calls to the same table faster.
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
