#' Create an object to access the nfi database
#'
#' @title nfi
#'
#' @export
#'
#' @examples
#' foo <- nfi()
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

      # get the cached data if any
      cached_data <- private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]]

      if (!rlang::is_null(cached_data)) {
        # return the cached data if exists
        return(cached_data)
      } else {
        # no cache, retrieve the data from db and update the cache
        res <- lfcdata:::.lfcproto_get_data(private, table_name, spatial)
        private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]] <- res
        return(res)
      }
    },

    # override default print
    print = function(...) {
      cat(
        "lfcNFI object to access the nfi database.\n",
        "foo <- lfcNFI$new(); foo$get_data('plots')"
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

#' access the nfi available tables
#'
#' @export
nfi_get_data <- function(object, table_name, spatial) {
  # argument validation
  # NOTE: table_name and spatial are validated in the method
  stopifnot(inherits(object, 'lfcNFI'))
  # call to the class method
  object$get_data(table_name, spatial)
}
