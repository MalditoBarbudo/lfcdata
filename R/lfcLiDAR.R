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
#'     \item{\code{$get_data}: Retrieve aggregated precalculated data for administrative
#'           divisions and natural areas. See \code{\link{lidar_get_data}} for more
#'           details}
#'     \item{\code{$get_lowres_raster}: Retrieve and collect LiDAR database rasters. See
#'           \code{\link{lidar_get_lowres_raster}} for more details}
#'     \item{\code{$avail_tables}: List all the tables that can be consulted. See
#'           \code{\link{lidar_avail_tables}} for more details}
#'     \item{\code{$describe_var}: Describe the variables, with their units and details.
#'           See \code{\link{lidar_describe_var}} for more details}
#'     \item{\code{$clip_and_stats}: Clip the specified variables with the provided set of
#'           polygons and calculate the raw raster statistics for each polygon. See
#'           \code{\link{lidar_clip_and_stats}} for more details}
#'     \item{\code{$point_value}: Extract values from the raw raster for the desired
#'           points. See \code{\link{lidar_point_value}} for more details}
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
        " Access to the LiDAR database.\n",
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
        "Use " %+% crayon::yellow$bold("lidar_get_data") %+%
          " to access the administrative divisions aggregated data.\n",
        "Use " %+% crayon::yellow$bold("lidar_get_lowres_raster") %+%
          " to access access the low resolution rasters (400x400m).\n",
        "Use " %+% crayon::yellow$bold("lidar_avail_tables") %+%
          " to know which tables are available.\n",
        "Use " %+% crayon::yellow$bold("lidar_describe_var") %+%
          " to get the information available on the variables.\n",
        "Use " %+% crayon::yellow$bold("lidar_clip_and_stats") %+%
          " to summarise the raw raster (20x20m) by provided polygons.\n",
        "Use " %+% crayon::yellow$bold("lidar_point_value") %+%
          " to extract values from the raw raster (20x20m).\n",
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
        variables, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE', 'all')
      )
      check_if_in_for(
        table_name, self$avail_tables()
      )

      # variables
      if (any(variables == 'all')) {
        variables <- c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE')
      }
      regex_detection <- glue::glue("^", glue::glue_collapse(variables, sep = '|^'), "|^poly_km2$")

      # get the data, select the variables. Check first if cache exists
      cached_data <-
        private$data_cache[[table_name]] %||% {
          lidar_agg_data <- sf::st_read(private$pool_conn, table_name, as_tibble = TRUE)
          private$data_cache[[table_name]] <- lidar_agg_data
          lidar_agg_data
        }

      res <-
        cached_data |>
        dplyr::select(poly_id, dplyr::matches(regex_detection))

      return(res)
    },

    # get_lowres_raster method.
    # LiDAR db is a postgis db so we need to retrieve the 400x400 raster table.
    # 2023-04-21: rpostgis will be retired, so we implement a function to read rasters from
    # postgis (get_raster_from_db)
    get_lowres_raster = function(variables, spatial = 'stars', rast_column = 'rast', clip = NULL) {

      # argument validation
      check_args_for(
        character = list(variables = variables, spatial = spatial)
      )
      check_length_for(spatial, 1)
      check_if_in_for(spatial, c('stars', 'raster'))
      check_if_in_for(
        variables, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE')
      )

      # chache name, as to avoid caching the same if the same tables, but in
      # different order, are provided
      cache_name <- glue::glue(
        "raster_{rlang::hash(variables)}{rlang::hash(clip)}"
        # glue::glue_collapse(variables |> sort(), sep = '_'), '_raster'
      )

      # check cache, retrieve it or make the query
      res <- private$data_cache[[cache_name]] %||% {
        variables_as_numbers <-
          variables |>
          sort() |>
          purrr::map_int(
            ~ switch(
              .x,
              'AB' = 1L, 'BAT' = 8L, 'BF' = 6L, 'CAT' = 9L,
              'DBH' = 2L, 'HM' = 4L, 'REC' = 7L, 'VAE' = 10L,
              'DEN' = 3L, 'LAI' = 5L
            )
          )

        message(
          'Querying low res (400x400m) raster from LFC database',
          ', this can take a while...'
        )
        # let's try to get the raster. With any error, the pool checkout is
        # not returned resulting in dangling db connections, so we use `try``
        lidar_raster <- try(
          get_raster_from_db(
            private$pool_conn, table_name = "lidar_stack_utm",
            rast_column = rast_column, bands = variables_as_numbers, clip = clip
          )
        )
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
        res <- res |>
          stars::st_as_stars()
        # we need to split to convert layers to attributes in case more
        # than one band is retrieved
        if (length(variables) > 1) {
          res <- res |> split("band")
        }
      }

      # return the raster
      return(res)
    },

    # available tables method
    avail_tables = function() {
      c(
        'lidar_catalonia', 'lidar_provinces', 'lidar_vegueries', 'lidar_counties',
        'lidar_municipalities',
        'lidar_pein', 'lidar_enpes', 'lidar_xn2000'
      )
    },

    # describe method
    describe_var = function(variables) {

      # argument checks
      check_args_for(character = list(variables = variables))
      check_if_in_for(variables, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE'))

      # cats
      lidar_describe_var_cat(
        variables, dplyr::tbl(private$pool_conn, 'variables_thesaurus')
      )

      # as the print method, to allow $ piping
      return(invisible(self))

    },

    # clip method
    clip_and_stats = function(sf, polygon_id_variable, variables) {

      # variables
      if (any(variables == 'all')) {
        variables <- c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE')
      }

      # res
      res <-
        variables |>
        purrr::map(
          ~ private$clip_and_stats_vectorized_for_polys(sf, polygon_id_variable, .x)
        ) |>
        purrr::reduce(
          .f = dplyr::full_join,
          by = c(polygon_id_variable, 'poly_km2')
        ) |>
        dplyr::left_join(sf, by = polygon_id_variable) |>
        sf::st_as_sf()
      return(res)
    },

    # point method
    point_value = function(sf, point_id_variable, variables) {

      # variables
      if (any(variables == 'all')) {
        variables <- c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE')
      }

      # res
      res <-
        variables |>
        purrr::map(
          ~ private$point_value_vectorized(sf, point_id_variable, .x)
        ) |>
        purrr::reduce(
          .f = dplyr::full_join,
          by = c(point_id_variable)
        ) |>
        dplyr::left_join(sf, by = point_id_variable) |>
        sf::st_as_sf()
      return(res)
    }
  ),
  # private methods and values
  private = list(
    # connection values
    dbname = 'lidargis',

    #### point_value and clip_and_stats intermediate methods

    # clip and mean for one polygon, one raster
    # we build a query to get the ST_SummaryStats of the raster values where the polygon
    # intersect. After that we summarise to get the stats, using cochrane for calculate
    # the sd
    clip_and_stats_simple_case = function(sf, poly_id, var_name) {

      # argument checks
      check_args_for(
        character = list(poly_id = poly_id, var_name = var_name),
        polygons = list(sf = sf)
      )
      check_if_in_for(var_name, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE'))
      check_length_for(var_name, 1)
      check_length_for(poly_id, 1)


      cat(crayon::green$bold(
        glue::glue("Processing {poly_id} polygon for {var_name} raster...")
      ))

      # var name to lowercase
      var_name <- tolower(var_name)

      # crs of the original sf
      original_crs <- sf::st_crs(sf)

      # poly as wkt, to avoid table creation
      sf_transformed <-
        sf |>
        sf::st_geometry() |>
        sf::st_transform(crs = 3043)

      wkt_poly <-
        sf_transformed |>
        sf::st_as_text(EWKT = TRUE, digits = 15)

      # poly area, in 25831 projection
      poly_area <- as.numeric(sf::st_area(sf_transformed)) / 1000000

      # pool checkout
      # pool_checkout <- pool::poolCheckout(private$pool_conn)

      # feature query. In this query we create the simple feature table-like
      feat_query <- glue::glue_sql(
        "SELECT {poly_id} As poly_id, ST_GeomFromEWKT({wkt_poly}) As geometry",
        .con = private$pool_conn
      )

      # stats query. In this query, IIUC, we join the raster to the feature on the tiles
      # intersecting, and we calculate the summary stats for the tiles. We return this, as
      # in this way we can calculate not only the mean, but also the std deviation.
      b_stats_query <- glue::glue_sql(
        "SELECT
           poly_id,
           (ST_SummaryStatsAgg(ST_Clip(rast,geometry, -9999, true),1,true)).*
         FROM
           {`var_name`},
           ({feat_query}) AS feat
         WHERE
           ST_Intersects(rast, geometry)
         GROUP BY poly_id;",
        .con = private$pool_conn
      )

      # pool::poolReturn(pool_checkout)

      # execute the query and retrieve the data
      # polygon_stats_pre_check <-
      polygon_stats <-
        pool::dbGetQuery(private$pool_conn, b_stats_query) |>
        # sf::st_read(
        #   private$pool_conn, query = b_stats_query, as_tibble = TRUE
        # ) |>
        # sf::st_transform(crs = original_crs) |>
        dplyr::as_tibble() |>
        dplyr::mutate(count = as.integer(count)) |>
        # dplyr::filter(count > 0) |>
        # dplyr::group_by(poly_id) |>
        dplyr::rename(
          # stats
          !! glue::glue("{toupper(var_name)}_pixels") := count,
          !! glue::glue("{toupper(var_name)}_average") := mean,
          !! glue::glue("{toupper(var_name)}_min") := min,
          !! glue::glue("{toupper(var_name)}_max") := max,
          !! glue::glue("{toupper(var_name)}_sd") := stddev,
        ) |>
        dplyr::mutate(
          # area of the polygon
          poly_km2 = poly_area,
          # area covered by raster (km2). Each pixel 20x20m=400m2=4e-04km2
          !! glue::glue("{toupper(var_name)}_km2") :=
            !! rlang::sym(glue::glue("{toupper(var_name)}_pixels")) * 4e-04,
          # prop of poly area covered by raster
          !! glue::glue("{toupper(var_name)}_km2_perc") :=
            100 * !! rlang::sym(glue::glue("{toupper(var_name)}_km2")) / poly_km2

        ) |>
        dplyr::select(
          poly_id, poly_km2, everything(), -sum
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
      check_if_in_for(id_var_name, names(sf))

      # get the geom column name
      sf_column <- attr(sf, 'sf_column')
      # rowbinding the summarises
      summ_polys_data <-
        seq_along(sf[[sf_column]]) |>
        purrr::map_dfr(
          ~ private$clip_and_stats_simple_case(
            sf = sf[[sf_column]][.x], poly_id = sf[[id_var_name]][.x],
            var_name = var_name
          )
        ) |>
        dplyr::rename(!! id_var_name := poly_id)

      return(summ_polys_data)
    },

    point_value_simple_case = function(sf, point_id, variable) {

      # argument checks
      check_args_for(
        character = list(point_id = point_id, variable = variable),
        points = list(sf = sf)
      )
      check_length_for(point_id, 1, 'point_id')
      check_length_for(variable, 1, 'variable')
      check_if_in_for(variable, c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE'))

      # we need the point in wkt to create the query on the fly
      wkt_point <-
        sf |>
        sf::st_geometry() |>
        sf::st_as_text(EWKT = TRUE)

      # var name
      var_name <- tolower(variable)

      # pool checkout
      # pool_checkout <- pool::poolCheckout(private$pool_conn)

      # SQL query
      point_query <- glue::glue_sql(
        "SELECT {point_id} As point_id, ST_Value(
           rast,
           ST_Transform(ST_GeomFromEWKT({wkt_point}),25831)
         ) As point_val
         FROM {`var_name`}
         WHERE ST_Intersects(
           rast,
           ST_Transform(ST_GeomFromEWKT({wkt_point}),25831)
         );",
        .con = private$pool_conn
      )

      # pool::poolReturn(pool_checkout)

      # execute the query and return the result
      res <-
        pool::dbGetQuery(private$pool_conn, statement = point_query) |>
        dplyr::as_tibble() |>
        dplyr::rename(!! variable := point_val)
      return(res)
    },

    point_value_vectorized = function(sf, id_point_variable, variable) {

      # argument check
      check_args_for(
        character = list(id_point_variable = id_point_variable, variable = variable),
        sf = list(sf = sf)
      )
      check_length_for(variable, 1, 'variable')
      check_length_for(id_point_variable, 1, 'id_point_variable')
      check_if_in_for(
        id_point_variable,
        names(sf |> dplyr::as_tibble() |> dplyr::select(-geometry))
      )

      # get the geom column name
      sf_column <- attr(sf, 'sf_column')
      # rowbinding the values
      points_data <-
        seq_along(sf[[sf_column]]) |>
        purrr::map_dfr(
          ~ private$point_value_simple_case(
            sf = sf[[sf_column]][.x], point_id = sf[[id_point_variable]][.x],
            variable = variable
          )
        ) |>
        dplyr::rename(!! id_point_variable := point_id)

      return(points_data)
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
#' @param variables character vector indicating variables for which data is returned. If
#'   not provided, all variables stats are returned
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
lidar_get_data <- function(
  object, table_name, variables = c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE')
) {
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
#' @param variables character vector indicating the requested raster/s variables.
#'   \code{"all"} can be used to retrieve all variables.
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
#'   in a temporary cache inside the lfcLiDAR object created by \code{\link{lidar}},
#'   making subsequent calls to the same table are faster. But, be warned that in-memory
#'   rasters can use a lot of memory!
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
#'   lidardb |>
#'     lidar_get_lowres_raster('AB', 'raster')
#'
#'   # or retrieve several tables at one time
#'   lidardb |>
#'     lidar_get_lowres_raster(c('AB', 'DBH'), 'stars')
#'
#'   # lidardb is an R6 object, so the previous examples are the same as:
#'   lidardb$get_lowres_raster('AB', 'raster')
#'   lidardb$get_lowres_raster('AB', 'stars')
#' }
#'
#' @export
lidar_get_lowres_raster <- function(object, variables, spatial = 'stars', rast_column = 'rast', clip = NULL) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$get_lowres_raster(variables, spatial, rast_column, clip)
}

#' Get the available tables in LiDAR db
#'
#' @description \code{lidar_avail_tables} is a wrapper for the \code{$avail_tables} method
#'   of \code{lfcLiDAR} objects. See \code{\link{lidar}}.
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
#' @description \code{lidar_describe_var} is a wrapper for the \code{$describe_var} method
#'   of \code{lfcLiDAR} objects. See \code{\link{lidar}}.
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

#' Clip by polygons and calculate stats from raw raster tables (20x20m)
#'
#' @description \code{lidar_clip_and_stats} is a wrapper for the \code{$clip_and_stats}
#'   method of \code{lfcLiDAR} objects. See \code{\link{lidar}}.
#'
#' @param object \code{lfcLiDAR} object, as created by \code{\link{lidar}}
#' @param sf sf object with the polygon/s to clip
#' @param polygon_id_variable character vector of length 1 with the name of the
#'   variable of \code{sf} that contains the polygon identificator (name, code...)
#' @param variables character vector with the names of the variables to access
#'
#' @return This function returns the same sf object provided with new columns with the
#'   mean of each polygon for each variable requested.
#'
#' @details
#'
#' The stats returned are the following:
#' \itemize{
#'   \item{\code{poly_km2}: Area in square kilometers of the polygon supplied}
#'   \item{\code{pixels}: Pixel count intersecting with the polygon supplied. Only pixels
#'         with value are counted}
#'   \item{\code{average}: Average value for the pixels intersecting with the polygon
#'         supplied}
#'   \item{\code{min}: Minimum value for the pixels intersecting with the polygon
#'         supplied}
#'   \item{\code{max}: Maximum value for the pixels intersecting with the polygon
#'         supplied}
#'   \item{\code{sd}: Standard deviation value for the pixels intersecting with the
#'         polygon supplied}
#'   \item{\code{km2}: Area covered by the raster intersecting with the polygon supplied}
#'   \item{\code{km2_perc}: Percentage of the supplied polygon area covered by the raster}
#' }
#'
#' @family LiDAR functions
#'
#' @examples
#' if (interactive()) {
#' library(dplyr)
#' lidardb <- lidar()
#'
#' polygons_data <- lidar_get_data('lidar_provincias', 'DBH') |>
#'   select(poly_id, DBH_check = DBH_mean, geometry)
#'
#' dbh_provinces <- lidar_clip_and_stats(lidardb, polygons_data, 'poly_id', 'DBH')
#' dbh_provinces$DBH_check == dbh_provinces$DBH_mean
#'
#' # lidardb is an R6 object, so the previous example is the same as:
#' lidardb$clip_and_stats(polygons_data, 'poly_id', 'DBH')
#' }
#'
#' @export
lidar_clip_and_stats <- function(object, sf, polygon_id_variable, variables) {
  # argument validation
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$clip_and_stats(sf, polygon_id_variable, variables)
}

#' Extract point value from LiDAR raw raster tables (20x20m)
#'
#' @description \code{lidar_point_value} is a wrapper for the \code{$point_value}
#'   method of \code{lfcLiDAR} objects. See \code{\link{lidar}}.
#'
#' @param object \code{lfcLiDAR} object, as created by \code{\link{lidar}}
#' @param sf sf object with the points to extract
#' @param point_id_variable character vector of length 1 with the name of the
#'   variable of \code{sf} that contains the point identificator (name, code...)
#' @param variables character vector with the names of the variables to access
#'
#' @return This function returns the same sf object provided, with new columns with the
#'   values of each point for each variable requested.
#'
#' @family LiDAR functions
#'
#' @examples
#' if (interactive()) {
#' library(dplyr)
#' lidardb <- lidar()
#'
#' points_data <-
#'   nfi()$get_data('plots', spatial = TRUE) |>
#'   dplyr::slice(1:5) |>
#'   dplyr::select(plot_id)
#'
#' dbh_plots <- lidar_point_value(lidardb, points_data, 'plot_id', 'DBH')
#'
#' # lidardb is an R6 object, so the previous example is the same as:
#' lidardb$point_value(points_data, 'plot_id', 'DBH')
#' }
#'
#' @export
lidar_point_value <- function(object, sf, point_id_variable, variables) {
  # argument validation
  check_class_for(object, 'lfcLiDAR')
  # call to the class method
  object$point_value(sf, point_id_variable, variables)
}
