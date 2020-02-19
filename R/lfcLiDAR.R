#' @description \code{lidar()} creates an object to access the LiDAR database.
#'
#' @title lfcLiDAR class
#'
#' @return An \code{lfcLiDAR} class object (inherits from \code{\link[R6]{R6Class}}),
#'   with methods to access the data. See Methods section.
#'
#' @section Methods:
#'   \code{lfcLiDAR} objects has the following methods available:
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
        "Use " %+% crayon::yellow$bold("lidar_clip_and_mean") %+%
          " to summarise the raster by provided polygons.\n",
        "See " %+%
          crayon::yellow$bold("vignette('tables_and_variables', package = 'lfcdata')") %+%
          " to learn more about the tables and variables."
      )
      invisible(self)
    },

    # get data method. This access to the precalculated data for administrative and
    # natural areas data. We need to overrride the super$get_data method to return
    # the spatial object
    get_data = function(table_name, variables) {

      # argument checks
      check_args_for(
        character = list(table_name = table_name, variables = variables)
      )
      check_length_for(table_name, 1)
      check_if_in_for(
        variables, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
      )

      # variables
      variables <- tolower(variables)
      regex_detection <- glue::glue(glue::glue_collapse(variables, sep = '$|'), "$")

      # get the data, select the variables. Check first if cache exists
      cached_data <-
        private$data_cache[[table_name]] %||% {
          lidar_agg_data <- sf::st_read(private$pool_conn, table_name, as_tibble = TRUE)
          private$data_cache[[table_name]] <- lidar_agg_data
          lidar_agg_data
        }

      res <-
        cached_data %>%
        dplyr::select(dplyr::matches(regex_detection))

      return(res)
    },

    # get_lowres_raster method.
    # LiDAR db is a postgis db so we need to access with rpostgis and retrieve the
    # 400x400 raster table.
    get_lowres_raster = function(variables, spatial = 'stars') {

      # argument validation
      check_args_for(
        character = list(variables = variables, spatial = spatial)
      )
      check_length_for(spatial, 1)
      check_if_in_for(spatial, c('stars', 'raster'))
      check_if_in_for(
        variables, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
      )

      # chache name, as to avoid caching the same if the same tables, but in
      # different order, are provided
      cache_name <- glue::glue(
        glue::glue_collapse(variables %>% sort(), sep = '_'), '_raster'
      )

      # check cache, retrieve it or make the query
      res <- private$data_cache[[cache_name]] %||% {
        variables_as_numbers <-
          variables %>%
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
        message(
          'Querying low res (400x400m) raster from LFC database',
          ', this can take a while...'
        )
        # let's try to get the raster. With any error, the pool checkout is
        # not returned resulting in dangling db connections, so we use `try``
        lidar_raster <- try(
          rpostgis::pgGetRast(
            temp_postgresql_conn, c('public', 'lidar_stack_utm'),
            bands = variables_as_numbers
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
        if (length(variables) > 1) {
          res <- res %>% split("band")
        }
      }

      # return the raster
      return(res)
    },

    # available tables method
    avail_tables = function() {
      c(
        'lidar_catalunya', 'lidar_provincias', 'lidar_veguerias', 'lidar_comarcas',
        'lidar_municipios'
      )
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
    clip_and_stats = function(sf, id_var_name, var_names) {
      res <-
        var_names %>%
        purrr::map(
          ~ private$clip_and_stats_vectorized_for_polys(sf, id_var_name, .x)
        ) %>%
        purrr::reduce(
          .f = dplyr::full_join,
          by = c(id_var_name, 'poly_km2')
        ) %>%
        dplyr::left_join(sf, by = id_var_name) %>%
        sf::st_as_sf()
      return(res)
    }

    # clip_and_mean = function(sf, table_name, safe = TRUE) {
    #   # @param table_name table name
    #   # @param sf sf object with the polygons to clip
    #   # @param safe logical indicating if memory and time safeguards are active
    #
    #   # argument checks
    #   check_args_for(
    #     sf = list(sf = sf),
    #     character = list(table_name = table_name),
    #     logical = list(safe = safe)
    #   )
    #   check_if_in_for(table_name, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE'))
    #
    #   # load the temp sf table (check for geom instead of geometry)
    #   user_polygons <-
    #     sf %>%
    #     sf::st_transform(crs = 3043) %>%
    #     sf::st_set_crs(3043)
    #
    #   # if safeguards are active, enforce them.
    #   if (isTRUE(safe)) {
    #     # area check
    #     user_area <- sf::st_area(user_polygons) %>% sum() %>% as.numeric()
    #     if (user_area > 500000000) {
    #       stop(glue::glue(
    #         'Polygon area (or polygons sum of areas) are above the maximum value',
    #         ' ({round(user_area/1000000, 1)} > 500 km2)'
    #       ))
    #     }
    #     # feature number
    #     user_features <- sf::st_geometry(user_polygons) %>% length()
    #     if (user_features > 10) {
    #       stop(glue::glue(
    #         'Number of features (polygons) is above the maximum value',
    #         ' ({user_features} > 10 polygons)'
    #       ))
    #     }
    #   }
    #
    #   # ok, cutting to the cheese. We need to clip the polygons, and after that calculate
    #   # the mean value for the left raster
    #   calculate_poly_mean <- function(data, raster_table) {
    #
    #     # get the geom column name
    #     sf_column <- attr(data, 'sf_column')
    #
    #     # This Eder Pebezsma snippet from print.stars method seems the way
    #     # to allow multiple attributes checking in a fast way:
    #     # as.data.frame(
    #     #   lapply(foo, function(y) structure(y, dim = NULL)),
    #     #   optional = TRUE
    #     # )
    #     # So, we need to iterate for each attribute (lapply/purrr) and remove the
    #     # dim attrb (structure), resulting in a list of each attribute
    #     # containing a vector of all cell values that we transform in a
    #     # dataframe and summarise all
    #     means_data <-
    #       seq_along(data[[sf_column]]) %>%
    #       purrr::map_dfr(
    #         .f = ~ sf::st_crop(raster_table, data[[sf_column]][.x]) %>%
    #           purrr::map(~ structure(.x, dim = NULL)) %>%
    #           tibble::as_tibble() %>%
    #           dplyr::summarise_all(.funs = mean, na.rm = TRUE)
    #       )
    #
    #     # now we join the means for each polygon (rows) and each attribute
    #     # (columns) with the polygons data
    #     res <- dplyr::bind_cols(data, means_data) # pun intended
    #     # return the updated data
    #     return(res)
    #   }
    #
    #   return(calculate_poly_mean(user_polygons, self$get_data(table_name)))
    # }
  ),
  # private methods and values
  private = list(
    # connection values
    dbname = 'lidargis',

    #### clip_and_stats intermediate methods

    # clip and mean for one polygon, one raster
    # we build a query to get the ST_SummaryStats of the raster values where the polygon
    # intersect. After that we summarise to get the stats, using cochrane for calculate the sd
    clip_and_stats_simple_case = function(sf, poly_id, var_name) {

      # argument checks
      check_args_for(
        character = list(poly_id = poly_id, var_name = var_name)
      )
      check_if_in_for(var_name, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE'))
      check_length_for(var_name, 1)
      check_length_for(poly_id, 1)


      cat(crayon::green$bold(
        glue::glue("Processing {poly_id} polygon for {var_name} raster...")
      ))

      # poly as wkt, to avoid table creation
      wkt_poly <-
        sf %>%
        sf::st_geometry() %>%
        sf::st_as_text(EWKT = TRUE)

      # var name to lowercase
      var_name <- tolower(var_name)

      # feature query. In this query we create the simple feature table-like
      feat_query <- glue::glue_sql(
        "SELECT {poly_id} As poly_id, ST_GeomFromEWKT({wkt_poly}) As geometry",
        .con = private$pool_conn
      )

      # stats query. In this query, IIUC, we join the raster to the feature on the tiles
      # intersecting, and we calculate the summary stats for the tiles. We return this, as
      # in this way we can calculate not only the mean, but also the std deviation.
      b_stats_query <- glue::glue_sql(
        "SELECT poly_id, geometry, (ST_SummaryStats(ST_Clip(rast,1,geometry, true),1,true)).*
         FROM {`var_name`}
       INNER JOIN ({feat_query}) AS feat
       ON ST_Intersects(feat.geometry,rast)",
        .con = private$pool_conn
      )

      # execute the query and retrieve the data
      intersecting_tiles_stats <- sf::st_read(
        private$pool_conn, query = b_stats_query, as_tibble = TRUE
      )

      polygon_stats <-
        intersecting_tiles_stats %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(count = as.integer(count)) %>%
        dplyr::filter(count > 0) %>%
        dplyr::group_by(poly_id) %>%
        dplyr::summarise(
          # area of the polygon
          poly_km2 = (sf::st_area(dplyr::first(.data[['geometry']])) %>% as.numeric()) / 1000000,
          # regular stats
          !! glue::glue("{toupper(var_name)}_pixels") := sum(.data[['count']]),
          !! glue::glue("{toupper(var_name)}_average") := sum(.data[['count']]*.data[['mean']])/sum(.data[['count']]),
          !! glue::glue("{toupper(var_name)}_min") := min(.data[['min']]),
          !! glue::glue("{toupper(var_name)}_max") := max(.data[['max']]),
          !! glue::glue("{toupper(var_name)}_sd") := cochrane_sd_reduce(
            n = .data[['count']], m = .data[['mean']], s = .data[['stddev']]
          ),
          # area covered by raster (km2). Each pixel 20x20m=400m2=4e-04km2
          !! glue::glue("{toupper(var_name)}_km2") := !! rlang::sym(glue::glue("{toupper(var_name)}_pixels")) * 4e-04,
          # prop of poly area covered by raster
          !! glue::glue("{toupper(var_name)}_km2_perc") := 100 * !! rlang::sym(glue::glue("{toupper(var_name)}_km2")) / poly_km2
        )

      cat(
        crayon::green$bold(glue::glue(" done.")), '\n'
      )

      return(polygon_stats)
    },

    # clip and mean vectorized for more than one polygon.
    # With map_dfr we build a dataframe with the statistics for each polygon supplied
    clip_and_stats_vectorized_for_polys = function(sf, id_var_name, var_name) {

      # argument checks (we only check for id_var_name, as the rest is gonna be
      # checked on clip_and_stats_simple_case)
      check_args_for(sf = list(sf = sf), character = list(id_var_name = id_var_name))
      check_length_for(id_var_name, 1)

      # get the geom column name
      sf_column <- attr(sf, 'sf_column')
      # rowbinding the summarises
      summ_polys_data <-
        seq_along(sf[[sf_column]]) %>%
        purrr::map_dfr(
          ~ private$clip_and_stats_simple_case(
            sf = sf[[sf_column]][.x], poly_id = sf[[id_var_name]][.x],
            var_name = var_name
          )
        )

      return(summ_polys_data)
    }
  )
)

#' Access the aggregated data for administrative and natural areas
#'
#' @description \code{lidar_get_data} is a wrapper for the \code{$get_data} method of
#'   \code{lfcLiDAR} objects. See also \code{\link{lidar}}.
#'
#' @param object \code{lfcLiDAR} object, as created by \code{\link{lidar}}
#' @param table_name character vector of lenght 1 indicating the table to retrieve
#' @param variables character vector indicating variables for which data is returned
#'
#' @return An sf object with the aggregated values for each administrative division or
#'   natural area for the variables requested
#'
#' @family LiDAR functions
#'
#' @details Precalculated aggregated values for
#'   \itemize{
#'     \item{Catalonia, in the \code{lidar_catalunya} table}
#'     \item{Provinces, in the \code{lidar_provincias} table}
#'     \item{Veguerias, in the \code{lidar_veguerias} table}
#'     \item{Regions, in the \code{lidar_comarcas} table}
#'     \item{Municipalities, in the \code{lidar_municipalities} table}
#'     \item{National Parks}
#'     \item{Natura 2000 Network}
#'   }
#'
#' @examples
#' if (interactive()) {
#'   lidardb <- lidar()
#'   # provinces data for DBH and AB
#'   provinces_data <- lidar_get_data(lidardb, 'lidar_provincias', c('AB', 'DBH'))
#'   provinces_data
#'
#'   # lidardb is an R6 object, so the previous example is the same as:
#'   lidardb$get_data('lidar_provincias', c('AB', 'DBH'))
#' }
#'
#' @export
lidar_get_data <- function(object, table_name, variables) {
  # argument validation
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$get_data(table_name, variables)
}

#' Access to the low resolution (400x400m) rasters in the LiDAR database
#'
#' @description \code{lidar_get_lowres_raster} is a wrapper for the
#'   \code{$get_lowres_raster} method of \code{lfcLiDAR} objects.
#'   See also \code{\link{lidar}}.
#'
#' @param object \code{lfcLiDAR} object, as created by \code{\link{lidar}}
#' @param variables character vector indicating the requested raster/s variables
#' @param spatial character vector of lenght 1 indicating the type of raster object to
#'   return, "raster" or "stars", the default.
#'
#' @return A raster object: \code{RasterLayer} if spatial is \code{raster} and only one
#'   variable is requested, \code{RasterBrick} if more than one variable is requested.
#'   \code{stars} if spatial is \code{stars}. See
#'   https://r-spatial.github.io/stars/index.html for details about stars objects and
#'   \code{\link[raster]{raster}} for details about raster objects.
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
#'   ab_raster <- lidar_get_lowres_raster(lidardb, 'AB', 'raster')
#'   # stars
#'   ab_stars <- lidar_get_lowres_raster(lidardb, 'AB', 'stars')
#'
#'   # we can use pipes
#'   lidardb %>%
#'     lidar_get_lowres_raster('AB', 'raster')
#'
#'   # or retrieve several tables at one time
#'   lidardb %>%
#'     lidar_get_lowres_raster(c('AB', 'DBH'), 'stars')
#'
#'   # lidardb is an R6 object, so the previous examples are the same as:
#'   lidardb$get_lowres_raster('AB', 'raster')
#'   lidardb$get_lowres_raster('AB', 'stars')
#' }
#'
#' @export
lidar_get_lowres_raster <- function(object, variables, spatial = 'stars') {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$get_lowres_raster(variables, spatial)
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
lidar_clip_and_stats <- function(object, sf, polygon_id_variable, table_names) {
  # argument validation
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$clip_and_stats(sf, polygon_id_variable, table_names)
}
