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
      check_args_for(character = list(table_name = table_name))
      check_length_for(table_name, 1)

      # return the cached data if exists. If no cache, retrieve the data from db
      # and update the cache
      # NOTE: %||% is in utils.R, simplifies the syntax and the readibility of the
      # expression.
      private$data_cache[[glue::glue("{table_name}_FALSE")]] %||% {
        # try to catch a db connection error
        message('Querying table from LFC database, this can take a while...')
        query_data <- try(
          dplyr::tbl(private$pool_conn, table_name) %>% dplyr::collect(),
          silent = TRUE
        )
        message('Done')
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
      res <- try({
        pool::dbPool(
          drv = RPostgres::Postgres(),
          dbname = private$dbname,
          host = 'laboratoriforestal.creaf.cat',
          idleTimeout = 3600,
          user = 'guest',
          password = 'guest',
          options = "-c client_min_messages=warning"
        )
      })

      if (inherits(res, "try-error")) {
        message(
          "Connection to database at laboratoriforestal.creaf.cat failed.",
          "Trying again in 30 seconds"
        )

        Sys.sleep(30)
        res <- pool::dbPool(
          drv = RPostgres::Postgres(),
          dbname = private$dbname,
          host = 'laboratoriforestal.creaf.cat',
          idleTimeout = 3600,
          user = 'guest',
          password = 'guest',
          options = "-c client_min_messages=warning"
        )
      }

      return(res)
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
