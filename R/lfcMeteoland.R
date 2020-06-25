#' @description \code{meteoland()} creates an object to access the Meteoland database.
#'
#' @title lfcMeteoland class
#'
#' @return An \code{lfcMeteoland} class object (inherits from \code{\link[R6]{R6Class}}),
#'   with methods to access the data. See Methods section.
#'
#' @section Methods:
#'   \code{lfcMeteoland} objects has the following methods available:
#'   \itemize{
#'     \item{\code{$get_data}: }
#'   }
#'
#' @family meteoland functions
#'
#' @export
#'
#' @examples
#' meteolanddb <- meteoland()
#' meteolanddb
meteoland <- function() {
  lfcMeteoland$new()
}

lfcMeteoland <- R6::R6Class(
  # specs
  classname = 'lfcMeteoland',
  inherit = lfcObject,
  cloneable = FALSE,
  # public methods
  public = list(
    # override the default print
    print = function(...) {
      cat(
        " Access to the Meteoland database.\n"#,
        # crayon::blue$underline("laboratoriforestal.creaf.uab.cat\n\n"),
        # "Use " %+% crayon::yellow$bold("lidar_get_data") %+%
        #   " to access the administrative divisions aggregated data.\n",
        # "Use " %+% crayon::yellow$bold("lidar_get_lowres_raster") %+%
        #   " to access access the low resolution rasters (400x400m).\n",
        # "Use " %+% crayon::yellow$bold("lidar_avail_tables") %+%
        #   " to know which tables are available.\n",
        # "Use " %+% crayon::yellow$bold("lidar_describe_var") %+%
        #   " to get the information available on the variables.\n",
        # "Use " %+% crayon::yellow$bold("lidar_clip_and_stats") %+%
        #   " to summarise the raw raster (20x20m) by provided polygons.\n",
        # "Use " %+% crayon::yellow$bold("lidar_point_value") %+%
        #   " to extract values from the raw raster (20x20m).\n",
        # "See " %+%
        #   crayon::yellow$bold("vignette('tables_and_variables', package = 'lfcdata')") %+%
        #   " to learn more about the tables and variables."
      )
      invisible(self)
    },

    # current points interpolation
    points_interpolation = function(sf, user_dates, .topo = NULL) {

      # argument checks are done in the ancillary functions, except for sf and
      # topo
      check_args_for(sf = list(sf = sf))
      check_length_for(user_dates, 2, 'user_dates')

      # get user topo
      if (is.null(.topo)) {
        user_topo <- private$get_points_topography(sf)
      } else {
        # check .topo class
        check_for_topo <- is(.topo, 'SpatialPointsTopography')

        if (!check_for_topo) {
          stop(".topo is not a SpatialPointsTopography object")
        }

        user_topo <- .topo
      }

      # get the interpolator
      interpolator <- private$build_points_interpolator(user_dates)

      # default parameters
      default_params <- meteoland::defaultInterpolationParams()
      buffer_days <- max(
        default_params$St_Precipitation, default_params$St_TemperatureRange
      )

      interpolation_points <-
        1:length(user_topo@coords[,1]) %>%
        purrr::map(
          function(index_coord) {
            meteoland::interpolationpoints(
              object = interpolator,
              points = user_topo[index_coord, ],
              verbose = FALSE
            )@data[[1]][-c(1:buffer_days), ] # remove the buffer days
          }
        )

      # finally, perform the interpolation
      res <- meteoland::SpatialPointsMeteorology(
        points = user_topo,
        data = interpolation_points,
        dates = interpolator@dates[-c(1:buffer_days)]
      )

      return(res)
    },

    # current raster interpolation
    raster_interpolation = function(sf, user_dates) {

      # argument checks
      check_length_for(user_dates, 2, 'user_dates')

      # This method iterate by dates to get the final rasters, as a list
      # with one element for each date supplied

      # datevec from user dates
      user_dates <- as.Date(user_dates)

      # previously to create the datevec, we must ensure end date is bigger than
      # start date
      if (! user_dates[[2]] > user_dates[[1]]) {
        stop('end date must be more recent than the start date')
      }


      datevec <-
        user_dates[[1]]:user_dates[[2]] %>%
        as.Date(format = '%j', origin = as.Date('1970-01-01'))

      res_list <-
        datevec %>%
        purrr::map(
          ~ private$raster_interpolation_vectorized_for_polys(sf, .x)
        )

      return(res_list)

    }

  ), # end of public methods
  # private methods
  private = list(
    # connection values
    dbname = 'meteoland',

    # point value helper methods ####
    # user topography
    get_points_topography = function(sf) {

      # argument checks
      check_args_for(
        points = list(sf = sf)
      )

      # we need here to transform the coordinates to UTM, reach the topography
      # raster in the db, get the value vectors for each variable (elevation,
      # aspect, slope) and use the meteoland SpatialPointsTopography function
      # to get the topography object

      # Transform the coordinates, We need sp for meteoland, wkt for getting
      # the values from the db.
      user_coords <-
        sf %>%
        sf::st_geometry() %>%
        sf::st_transform(
          crs = 3043
        )

      user_coords_sp <-
        user_coords %>%
        # sf::st_transform(
        #   crs = "+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +towgs84=0,0,0"
        # ) %>%
        sf::as_Spatial()

      user_coords_wkt <-
        user_coords %>%
        sf::st_as_text(EWKT = TRUE)

      # Get db raster values
      # pool checkout
      pool_checkout <- pool::poolCheckout(private$pool_conn)

      # browser()
      # SQL queries
      point_queries <-
        user_coords_wkt %>%
        purrr::map(
          ~ glue::glue_sql(
            "SELECT ST_Value(
              rast, ST_Transform(ST_GeomFromEWKT({.x}),3043)
            ) As elevation,
              ST_Value(
              rast, 2, ST_Transform(ST_GeomFromEWKT({.x}),3043)
              ) As slope,
              ST_Value(
              rast, 3, ST_Transform(ST_GeomFromEWKT({.x}),3043)
              ) As aspect,
            {.x} As coords_text
            FROM topology_cat
            WHERE ST_Intersects(
              rast,
              ST_Transform(ST_GeomFromEWKT({.x}),3043)
            );",
            .con = pool_checkout
          )
        )

      # return the checkout, we don't want ghost db connections
      pool::poolReturn(pool_checkout)

      query_helper <- function(query) {
        query_res <- pool::dbGetQuery(private$pool_conn, statement = query)
        if (nrow(query_res) > 1) {
          query_res <-
            query_res %>%
            dplyr::filter(!is.na(elevation), !is.na(aspect), !is.na(slope))
        }

        if (nrow(query_res) < 1) {
          query_res <- data.frame(
            elevation = NA_real_, slope = NA_real_, aspect = NA_real_,
            coords_text = NA_character_
          )
        }
        return(query_res)
      }

      query_helper <- purrr::possibly(
        query_helper,
        otherwise = data.frame(
          elevation = NA_real_, slope = NA_real_, aspect = NA_real_,
          coords_text = NA_character_
        )
      )

      # execute the query
      raster_topography_values <-
        point_queries %>%
        purrr::map_dfr(query_helper)


      # build the topography object
      user_topo <- meteoland::SpatialPointsTopography(
        points = user_coords_sp,
        elevation = raster_topography_values[['elevation']],
        slope = raster_topography_values[['slope']],
        aspect = raster_topography_values[['aspect']]
      )

      return(user_topo)
    },

    # meteoland interpolator
    build_points_interpolator = function(user_dates) {

      # argument checks
      check_args_for(
        character = list(user_dates = user_dates),
        date = list(user_dates = user_dates)
      )

      # default parameters
      default_params <- meteoland::defaultInterpolationParams()

      buffer_days <- max(
        default_params$St_Precipitation, default_params$St_TemperatureRange
      )

      # build the dates vector to read the metereology tables
      user_dates <- as.Date(user_dates)
      datevec <-
        (user_dates[[1]] - buffer_days):user_dates[[2]] %>%
        as.Date(format = '%j', origin = as.Date('1970-01-01'))
      table_names <-
        glue::glue("daily_meteo_{stringr::str_remove_all(datevec, '-')}") %>%
        magrittr::extract(. %in% dplyr::db_list_tables(private$pool_conn))

      # meteo data
      # TODO what happens when no table is found?????? We need to check this
      # and avoid the error, just maybe purrr::possibly or similar
      meteo_data <-
        table_names %>%
        purrr::map(
          ~ dplyr::tbl(private$pool_conn, .x) %>%
            dplyr::collect() %>%
            # essential to cross results with meteo stations:
            as.data.frame() %>%
            magrittr::set_rownames(.$stationCode)
        )

      # meteo stations info
      unique_meteo_stations <-
        meteo_data %>%
        purrr::map_dfr(
          function(df) {
            df %>%
              dplyr::select(stationCode, stationOrigin, lat, long, elevation)
          }
        ) %>%
        dplyr::distinct()

      interpolator_res <-
        unique_meteo_stations %>%
        dplyr::select(long, lat) %>%
        as.data.frame() %>%
        magrittr::set_rownames(unique_meteo_stations$stationCode) %>%
        sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
        sp::spTransform(sp::CRS(
          "+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
        )) %>%
        meteoland::SpatialPointsMeteorology(meteo_data, datevec, TRUE) %>%
        meteoland::MeteorologyInterpolationData(
          elevation = unique_meteo_stations$elevation,
          params = default_params
        )

      # Before returning the interpolator, we need to change some params based
      # on the latest calibrations
      latest_calibration <-
        dplyr::tbl(private$pool_conn, 'interpolation_parameters') %>%
        dplyr::filter(year == max(year, na.rm = TRUE)) %>%
        dplyr::collect()

      interpolator_res@params$N_MinTemperature <-
        latest_calibration$N_MinTemperature
      interpolator_res@params$alpha_MinTemperature <-
        latest_calibration$alpha_MinTemperature
      interpolator_res@params$N_MaxTemperature <-
        latest_calibration$N_MaxTemperature
      interpolator_res@params$alpha_MaxTemperature <-
        latest_calibration$alpha_MaxTemperature
      interpolator_res@params$N_DewTemperature <-
        latest_calibration$N_DewTemperature
      interpolator_res@params$alpha_DewTemperature <-
        latest_calibration$alpha_DewTemperature
      interpolator_res@params$N_PrecipitationEvent <-
        latest_calibration$N_PrecipitationEvent
      interpolator_res@params$alpha_PrecipitationEvent <-
        latest_calibration$alpha_PrecipitationEvent
      interpolator_res@params$N_PrecipitationAmount <-
        latest_calibration$N_PrecipitationAmount
      interpolator_res@params$alpha_PrecipitationAmount <-
        latest_calibration$alpha_PrecipitationAmount

      return(interpolator_res)
    },

    # current raster interpolation
    raster_interpolation_simple_case = function(sf_geom, date) {

      # argument check
      check_args_for(
        polygons = list(sf_geom = sf_geom),
        date = list(date = date)
      )

      # Interpolation for grids is not made on the fly, but from precalculated
      # 1km rasters instead. So we need to implement a similar method as the
      # one in lidar to clip and recover the clipped raster.

      # convert sf
      sf_spatial <- sf::as_Spatial(sf_geom)

      raster_table_name <- glue::glue(
        "daily_raster_interpolated_{stringr::str_remove_all(date, '-')}"
      )

      # pool checkout
      pool_checkout <- pool::poolCheckout(private$pool_conn)
      # get raster
      raster_cropped <- rpostgis::pgGetRast(
        pool_checkout, raster_table_name,
        bands = TRUE, boundary = sf_spatial
      )
      # close checkout
      pool::poolReturn(pool_checkout)

      # clip the raster
      raster_clipped <- raster::mask(raster_cropped, sf_spatial)

      return(raster_clipped)
    },

    raster_interpolation_vectorized_for_polys = function(sf, date) {

      # argument checks
      check_args_for(
        sf = list(sf = sf),
        date = list(date = date)
      )

      # get the geom column name
      sf_column <- attr(sf, 'sf_column')
      # rowbinding the summarises
      raster_merged <-
        seq_along(sf[[sf_column]]) %>%
        purrr::map(
          ~ private$raster_interpolation_simple_case(sf[[sf_column]][.x], date)
        ) %>%
        purrr::reduce(raster::merge)

      names(raster_merged) <- c(
        "MeanTemperature", "MinTemperature", "MaxTemperature",
        "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
        "Precipitation", "Radiation", "WindSpeed", "WindDirection"
      )

      return(raster_merged)

    }

  ) # end of private methods
)
