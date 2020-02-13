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
#'     \item{\code{$avail_tables}: List all the tables that can be consulted. See
#'           \code{\link{lidar_avail_tables}} for more details}
#'     \item{\code{$describe_var}: Describe the variables, with their units and details.
#'           See \code{\link{lidar_describe_var}} for more details}
#'     \item{\code{$clip_and_mean}: Clip the specified tables with the provided set of
#'           polygons and calculate the raster mean for each polygon. See
#'           \code{\link{lidar_clip_and_mean}} for more details}
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
        crayon::blue$underline("laboratoriforestal.creaf.uab.cat\n\n"),
        "Use " %+% crayon::yellow$bold("lidar_get_data") %+%
          " to access the tables.\n",
        "Use " %+% crayon::yellow$bold("lidar_avail_tables") %+%
          " to know which tables are available.\n",
        "Use " %+% crayon::yellow$bold("lidar_describe_var") %+%
          " to get the information available on the variables.\n",
        "See " %+%
          crayon::yellow$bold("vignette('tables_and_variables', package = 'lfcdata')") %+%
          " to learn more about the tables and variables."
      )
      invisible(self)
    },
    # get_data method. LiDAR db is a postgis db so we need to access with rpostgis and
    # override the super$get_data method.
    get_data = function(table_name, spatial = 'stars') {

      # argument validation
      check_args_for(
        character = list(table_name = table_name, spatial = spatial)
      )
      check_length_for(spatial, 1)
      check_if_in_for(spatial, c('stars', 'raster'))
      check_if_in_for(
        table_name, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
      )

      # chache name, as to avoid caching the same if the same tables, but in
      # different order, are provided
      cache_name <- glue::glue(
        glue::glue_collapse(table_name %>% sort(), sep = '_'), '_raster'
      )

      # check cache, retrieve it or make the query
      res <- private$data_cache[[cache_name]] %||% {
        table_name_as_numbers <-
          table_name %>%
          sort() %>%
          purrr::map_int(
            ~ switch(
              .x,
              'AB' = 1L, 'BAT' = 6L, 'BF' = 4L, 'CAT' = 7L,
              'DBH' = 2L, 'HM' = 3L, 'REC' = 5L, 'VAE' = 8L
            )
          )

        # temp persistent conn object (rpostgis not working with pool objects)
        temp_postgresql_conn <- pool::poolCheckout(private$pool_conn)
        message('Querying raster from LFC database, this can take a while...')
        # let's try to get the raster. With any error, the pool checkout is
        # not returned resulting in dangling db connections, so we use `try``
        lidar_raster <- try(
          rpostgis::pgGetRast(
            temp_postgresql_conn, c('public', 'lidar_stack_utm'),
            bands = table_name_as_numbers
          )
        )
        # return the pool checkout, before anything else
        pool::poolReturn(temp_postgresql_conn)
        # check if lidar_raster inherits from try-error to stop
        if (inherits(lidar_raster, "try-error")) {
          stop("Can not connect to the database:\n", lidar_raster[1])
        }

        message('Done')

        # update cache
        private$data_cache[[cache_name]] <- lidar_raster
        # return raster
        lidar_raster
      }

      # now we can return a raster (just as is) or a stars object
      if (spatial == 'stars') {
        res <- res %>%
          stars::st_as_stars()
        # we need to split to convert layers to attributes in case more
        # than one band is retrieved
        if (length(table_name) > 1) {
          res <- res %>% split("band")
        }
      }

      # return the raster
      return(res)



      # # argument validation
      # check_args_for(
      #   character = list(table_name = table_name, spatial = spatial),
      # )
      # check_length_for(table_name, 1)
      # check_length_for(spatial, 1)
      # check_if_in_for(spatial, c('stars', 'raster'))
      # check_if_in_for(
      #   table_name, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
      # )
      #
      # # check cache, retrieve it or make the query
      # res <- private$data_cache[[
      #   glue::glue("{table_name}_{as.character(spatial)}")
      # ]] %||%
      #   {
      #     table_name_as_number <- switch(
      #       table_name,
      #       'AB' = 1,
      #       'BAT' = 6,
      #       'BF' = 4,
      #       'CAT' = 7,
      #       'DBH' = 2,
      #       'HM' = 3,
      #       'REC' = 5,
      #       'VAE' = 8
      #     )
      #
      #     # temp persistent conn object (rpostgis not working with pool objects)
      #     temp_postgresql_conn <- pool::poolCheckout(private$pool_conn)
      #     message('Querying raster from LFC database, this can take a while...')
      #     # let's try to get the raster. With any error, the pool checkout is not returned
      #     # resulting in dangling db connections, so we use try
      #     lidar_raster <- try(
      #       rpostgis::pgGetRast(
      #         temp_postgresql_conn, c('public', 'lidar_stack_utm'),
      #         bands = table_name_as_number
      #       )
      #     )
      #     # return the pool checkout, before anything else
      #     pool::poolReturn(temp_postgresql_conn)
      #     # check if lidar_raster inherits from try-error to stop
      #     if (inherits(lidar_raster, "try-error")) {
      #       stop("Can not connect to the database:\n", lidar_raster[1])
      #     }
      #     # well, now we can return a raster (just as is) or a stars
      #     res <- switch(
      #       spatial,
      #       'raster' = lidar_raster,
      #       'stars' = lidar_raster %>% stars::st_as_stars()
      #     )
      #     message('Done')
      #     # update cache
      #     private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]] <- res
      #     res
      #   }
    },

    # available tables method
    avail_tables = function() {
      c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
    },

    # describe method
    describe_var = function(variables) {

      # argument checks
      check_args_for(character = list(variables = variables))
      check_if_in_for(variables, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE'))

      # cats
      lidar_describe_var_cat(
        variables, dplyr::tbl(private$pool_conn, 'variables_thesaurus')
      )

      # as the print method, to allow $ piping
      return(invisible(self))

    },

    # clip method
    clip_and_mean = function(sf, table_name, safe = TRUE) {
      # @param table_name table name
      # @param sf sf object with the polygons to clip
      # @param safe logical indicating if memory and time safeguards are active

      # argument checks
      check_args_for(
        sf = list(sf = sf),
        character = list(table_name = table_name),
        logical = list(safe = safe)
      )
      check_if_in_for(table_name, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE'))

      # load the temp sf table (check for geom instead of geometry)
      user_polygons <-
        sf %>%
        sf::st_transform(crs = 3043) %>%
        sf::st_set_crs(3043) %>% {
          temp_data <- .
          if ('geom' %in% names(temp_data)) {
            temp_data <- dplyr::rename(temp_data, geometry = geom)
          }
          temp_data
        }

      # if safeguards are active, enforce them.
      if (isTRUE(safe)) {
        # area check
        user_area <- sf::st_area(user_polygons) %>% sum() %>% as.numeric()
        if (user_area > 500000000) {
          stop(glue::glue(
            'Polygon area (or polygons sum of areas) are above the maximum value',
            ' ({round(user_area/1000000, 1)} > 500 km2)'
          ))
        }
        # feature number
        user_features <- sf::st_geometry(user_polygons) %>% length()
        if (user_features > 10) {
          stop(glue::glue(
            'Number of features (polygons) is above the maximum value',
            ' ({user_features} > 10 polygons)'
          ))
        }
      }

      # ok, cutting to the cheese. We need to clip the polygons, and after that calculate
      # the mean value for the left raster
      calculate_poly_mean <- function(data, raster_table) {

        # This Eder Pebezsma snippet from print.stars method seems the way
        # to allow multiple attributes checking:
        # as.data.frame(
        #   lapply(foo, function(y) structure(y, dim = NULL)),
        #   optional = TRUE
        # )
        # So, we need to iterate for each attribute (lapply/purrr) and remove the
        # dim attrb (structure), resulting in a list of each attribute
        # containing a vector of all cell values that we transform in a
        # dataframe and summarise all
        means_data <-
          seq_along(data[['geometry']]) %>%
          purrr::map_dfr(
            .f = ~ sf::st_crop(raster_table, data[['geometry']][.x]) %>%
              purrr::map(~ structure(.x, dim = NULL)) %>%
              tibble::as_tibble() %>%
              dplyr::summarise_all(.funs = mean, na.rm = TRUE)
          )

        # now we join the means for each polygon (rows) and each attribute
        # (columns) with the polygons data
        res <- dplyr::bind_cols(data, means_data)
        # return the updated data
        return(res)
      }

      return(calculate_poly_mean(user_polygons, self$get_data(table_name)))
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
#' @return A raster object: \code{RasterLayer} if spatial is \code{raster},
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
#' if (interactive()) {
#'   lidardb <- lidar()
#'   # raster
#'   ab_raster <- lidar_get_data(lidardb, 'AB', 'raster')
#'   # stars
#'   ab_stars <- lidar_get_data(lidardb, 'AB', 'stars')
#'
#'   # we can use pipes
#'   lidardb %>%
#'     lidar_get_data('AB', 'raster')
#'
#'   # lidardb is an R6 object, so the previous examples are the same as:
#'   lidardb$get_data('AB', 'raster')
#'   lidardb$get_data('AB', 'stars')
#' }
#'
#' @export
lidar_get_data <- function(object, table_name, spatial = 'stars') {
  # argument validation
  # NOTE: table_name and spatial are validated in the method
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$get_data(table_name, spatial)
}

#' Get the available tables in LiDAR db
#'
#' @description \code{lidar_avail_tables} is a wrapper for the \code{$avail_tables} method of
#'   \code{lfcLiDAR} objects. See \code{\link{lidar}}.
#'
#' @param object \code{lfcLiDAR} object, as created by \code{\link{lidar}}
#'
#' @return A character vector with the table names
#'
#' @family LiDAR functions
#'
#' @examples
#' if (interactive()) {
#'   lidardb <- lidar()
#'   lidar_avail_tables(lidardb)
#'
#'   # lidardb is an R6 object, so the previous example is the same as:
#'   lidardb$avail_tables()
#' }
#'
#' @export
lidar_avail_tables <- function(object) {
  # argument validation
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$avail_tables()
}

#' Print info about the variables present in the LiDAR db
#'
#' @description \code{lidar_describe_var} is a wrapper for the \code{$describe_var} method of
#'   \code{lfcLiDAR} objects. See \code{\link{lidar}}.
#'
#' @param object \code{lfcLiDAR} object, as created by \code{\link{lidar}}
#' @param variables character vector with the names of the variables to describe
#'
#' @return A character vector with the variable names to describe
#'
#' @family LiDAR functions
#'
#' @examples
#' if (interactive()) {
#' lidardb <- lidar()
#' lidar_describe_var(lidardb, "BF")
#' lidar_describe_var(lidardb, c("DBH", "VAE"))
#'
#' # lidardb is an R6 object, so the previous example is the same as:
#' lidardb$describe_var("BF")
#' lidardb$describe_var(c("DBH", "VAE"))
#' }
#'
#' @export
lidar_describe_var <- function(object, variables) {
  # argument validation
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$describe_var(variables)
}

#' Clip and calculate the mean of raster tables
#'
#' @description \code{lidar_clip_and_mean} is a wrapper for the \code{$clip_and_mean}
#'   method of \code{lfcLiDAR} objects. See \code{\link{lidar}}.
#'
#' @param object \code{lfcLiDAR} object, as created by \code{\link{lidar}}
#' @param sf sf object with the polygon/s to clip
#' @param table_names character vector with the names of the tables to access
#'
#' @return This function returns the same sf object provided with new columns with the
#'   mean of each polygon for each table requested.
#'
#' @family LiDAR functions
#'
#' @examples
#' if (interactive()) {
#' lidardb <- lidar()
#' }
#'
#' @export
lidar_clip_and_mean <- function(object, sf, table_names) {
  # argument validation
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$clip_and_mean(sf, table_names, safe = FALSE)
}
