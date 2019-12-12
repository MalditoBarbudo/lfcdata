#' @description \code{lidar()} creates an object to access the LiDAR database.
#'
#' @title lfcLiDAR class
#'
#' @return An \code{lfcLiDAR} class object (inherits from \code{\link[R6]{R6Class}}),
#'   with methods to access the data. See Methods section.
#'
#' @section Methods:
#'   \code{lfcLiDAR} objects has one public method:
#'   \itemize{
#'     \item{\code{$get_data}: Retrieve and collect LiDAR database rasters. See
#'           \code{\link{lidar_get_data}} for more details}
#'   }
#'
#' @family LiDAR functions
#'
#' @export
#'
#' @examples
#' lidardb <- lidar()
#' lidardb
lidar <- function() {
  lfcLiDAR$new()
}

lfcLiDAR <- R6::R6Class(
  # specs
  classname = 'lfcLiDAR',
  inherit = lfcObject,
  cloneable = FALSE,
  # public methods
  public = list(
    # override the default print
    print = function(...) {
      cat(
        " Access to the LiDAR rasters database.\n",
        "(laboratoriforestal.creaf.uab.cat)\n\n",
        "Use lidar_get_data to access the rasters.\n",
        "See vignette('tables_and_variables', package = 'lfcdata') to learn more about the tables and variables."
      )
      invisible(self)
    },
    # get_data method. LiDAR db is a postgis db so we need to access with rpostgis and
    # override the super$get_data method.
    get_data = function(table_name, spatial = 'stars') {

      # argument validation (table name is always validated in the super
      stopifnot(
        rlang::is_character(spatial),
        length(spatial) == 1,
        spatial %in% c('stars', 'raster')
      )

      # check cache, retrieve it or make the query
      res <- private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]] %||%
        {
          table_name_as_number <- switch(
            table_name,
            'AB' = 1,
            'BAT' = 6,
            'BF' = 4,
            'CAT' = 7,
            'DBH' = 2,
            'HM' = 3,
            'REC' = 5,
            'VAE' = 8
          )

          # check for table name
          stopifnot(
            table_name_as_number
          )

          # temp persistent conn object (rpostgis not working with pool objects)
          temp_postgresql_conn <- pool::poolCheckout(private$pool_conn)
          # let's try to get the raster. With any error, the pool checkout is not returned
          # resulting in dangling db connections, so we use try
          lidar_raster <- try(
            rpostgis::pgGetRast(
              temp_postgresql_conn, c('public', 'lidar_stack'), bands = table_name_as_number
            )
          )
          # return the pool checkout, before anything else
          pool::poolReturn(temp_postgresql_conn)
          # check if lidar_raster inherits from try-error to stop
          if (inherits(lidar_raster, "try-error")) {
            stop("Can not connect to the database:\n", lidar_raster[1])
          }
          # well, now we can return a raster (just as is) or a stars
          res <- switch(
            spatial,
            'raster' = lidar_raster,
            'stars' = lidar_raster %>% stars::st_as_stars()
          )
          # update cache
          private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]] <- res
          res
        }
    }
  ),
  # private methods and values
  private = list(
    # connection values
    dbname = 'lidargis'
  )
)

#' Access to the tables in the LiDAR database
#'
#' @description \code{lidar_get_data} is a wrapper for the \code{$get_data} method of
#'   \code{lfcLiDAR} objects. See also \code{\link{lidar}}.
#'
#' @param object \code{lfcLiDAR} object, as created by \code{\link{lidar}}
#' @param table_name character vector of lenght 1 indicating the requested raster name
#' @param spatial character vector of lenght 1 indicating the type of raster object to
#'   return, "raster" or "stars", the default.
#'
#' @return A raster object: \code{Raster} if spatial is \code{raster},
#'   \code{stars} if spatial is \code{stars}. See https://r-spatial.github.io/stars/index.html
#'   for details about stars objects and \code{\link[raster]{raster}} for details
#'   about raster objects.
#'
#' @family LiDAR functions
#'
#' @details Connection to database can be slow. Rasters retrieved from the db are stored
#'   in a temporary cache inside the lfcLiDAR object created by \code{\link{lidar}}, making
#'   subsequent calls to the same table are faster. But, be warned that in-memory rasters
#'   can use a lot of memory!
#'
#' @examples
#' lidardb <- lidar()
#' # raster
#' ab_raster <- lidar_get_data(lidardb, 'AB', 'raster')
#' # stars
#' ab_stars <- lidar_get_data(lidardb, 'AB', 'stars')
#'
#' # we can use pipes
#' lidardb %>%
#'   lidar_get_data('plots', 'raster')
#'
#' # lidardb is an R6 object, so the previous examples are the same as:
#' lidardb$get_data('AB', 'raster')
#' lidardb$get_data('AB', 'stars')
#'
#' @export
lidar_get_data <- function(object, table_name, spatial = 'stars') {
  # argument validation
  # NOTE: table_name and spatial are validated in the method
  stopifnot(inherits(object, 'lfcLiDAR'))
  # call to the class method
  object$get_data(table_name, spatial)
}
