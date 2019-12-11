lfcObject <- R6::R6Class(
  # specs
  classname = 'lfcObject',
  # public methods and values
  public = list(
    # initialize method
    initialize = function() {
      private$pool_conn <- private$pool_conn_create()
    },

    # get data method, children classes will use this or modify it as
    # desired
    get_data = function(table_name) {
      # arguments validation
      stopifnot(
        rlang::is_character(table_name) & length(table_name) == 1
      )

      # return the cached data if exists. If no cache, retrieve the data from db
      # and update the cache
      # NOTE: %||% is in utils.R, simplifies the syntax and the readibility of the
      # expression.
      private$data_cache[[glue::glue("{table_name}_FALSE")]] %||% {
        # try to catch a db connection error
        query_data <- try(
          dplyr::tbl(private$pool_conn, table_name) %>% dplyr::collect()
        )
        # check if any error
        if (inherits(query_data, "try-error")) {
          stop("Can not connect to the database:\n", query_data[1])
        } else {
          private$data_cache[[glue::glue("{table_name}_FALSE")]] <- query_data
          return(query_data)
        }
      }
    }
  ),

  # private methods and values
  private = list(
    # dbname
    dbname = NULL,

    # pool connection
    pool_conn = NULL,

    # initialize method function
    pool_conn_create = function() {
      pool::dbPool(
        drv = RPostgreSQL::PostgreSQL(),
        dbname = private$dbname,
        host = 'laboratoriforestal.creaf.uab.cat',
        idleTimeout = 3600000,
        user = 'guest',
        password = 'guest'
      )
    },

    # cache object
    data_cache = list(),

    # finalize method
    finalize = function() {
      # when object is collected or R session exits, close the db connections
      pool::poolClose(private$pool_conn)
    }
  )
)
