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

## lfcMeteoland Class ####
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
        " Access to the Meteoland database.\n",
        crayon::blue$underline("laboratoriforestal.creaf.uab.cat\n\n"),
        "Use " %+% crayon::yellow$bold("meteoland_point_interpolation") %+%
          " to interpolate points in the last 365 days (current mode).\n",
        "Use " %+% crayon::yellow$bold("meteoland_raster_interpolation") %+%
          " to interpolate polygons in the last 365 days (current mode).\n",
        "Use " %+% crayon::yellow$bold("meteoland_get_lowres_raster") %+%
          " to access access the low resolution rasters (1000x1000m).\n",
        "See " %+%
          crayon::yellow$bold("vignette('tables_and_variables', package = 'lfcdata')") %+%
          " to learn more about the tables and variables."
      )
      invisible(self)
    },

    get_data = function() {
      # here there is no tables to get, and the method must no go to the
      # super$get_data method, as there is no tables
      cat(
        crayon::red$bold("No get_data method available in this database")
      )
      invisible(self)
    },

    # current points interpolation
    # points_interpolation = function(sf, user_dates, points_id, .topo = NULL) {
    #
    #   # argument checks are done in the ancillary functions, except for sf and
    #   # topo
    #   check_args_for(
    #     sf = list(sf = sf),
    #     character = list(points_id = points_id)
    #   )
    #   check_length_for(user_dates, 2, 'user_dates')
    #
    #   message("Getting the topography")
    #   # get user topo
    #   if (is.null(.topo)) {
    #     message("By db")
    #     user_topo <- private$get_points_topography(sf)
    #   } else {
    #
    #     message("By provided topo")
    #     # check .topo class
    #     check_for_topo <- is(.topo, 'SpatialPointsTopography')
    #
    #     if (!check_for_topo) {
    #       stop(".topo is not a SpatialPointsTopography object")
    #     }
    #
    #     user_topo <- .topo
    #     # if the topo is provided, then we need to create the attribute of
    #     # offending coords, empty
    #     attr(user_topo, 'offending_coords') <- numeric(0)
    #   }
    #
    #   message("Getting the interpolator")
    #   # get the interpolator
    #   interpolator <- private$build_points_interpolator(user_dates)
    #
    #   # default parameters
    #   default_params <- meteoland::defaultInterpolationParams()
    #   buffer_days <- max(
    #     default_params$St_Precipitation, default_params$St_TemperatureRange
    #   )
    #
    #   message("Points interpolation")
    #   interpolation_points <-
    #     1:length(user_topo@coords[,1]) %>%
    #     purrr::map(
    #       function(index_coord) {
    #         message(
    #           "Interpolating point ", index_coord, " of ", length(user_topo@coords[,1])
    #         )
    #         meteoland::interpolationpoints(
    #           object = interpolator,
    #           points = user_topo[index_coord, ],
    #           verbose = FALSE
    #         )@data[[1]][-c(1:buffer_days), ] # remove the buffer days
    #       }
    #     )
    #
    #   message("pointsmeteorology creation")
    #   # finally, perform the interpolation
    #   res <- meteoland::SpatialPointsMeteorology(
    #     points = user_topo,
    #     data = interpolation_points,
    #     dates = interpolator@dates[-c(1:buffer_days)]
    #   )
    #
    #   message("Naming")
    #   # now we need to create the names of the list res@data. Each element is
    #   # a point, so, we need to take the names, remove the offending coords
    #   # and set the names.
    #   points_names <- sf %>%
    #     dplyr::filter(
    #       !dplyr::row_number() %in% attr(user_topo, 'offending_coords')
    #     ) %>%
    #     dplyr::pull(!! rlang::sym(points_id))
    #
    #   names(res@data) <- points_names
    #
    #   return(res)
    # },

    points_interpolation = function(sf, user_dates, points_id, .topo = NULL) {

      # argument checks are done in the ancillary functions, except for sf and
      # topo
      check_args_for(
        sf = list(sf = sf),
        character = list(points_id = points_id)
      )
      check_length_for(user_dates, 2, 'user_dates')

      # message("Getting the topography")
      # get user topo
      if (is.null(.topo)) {
        # message("By db")
        user_topo <- private$get_points_topography(sf)
      } else {

        # message("By provided topo")
        # check .topo class
        check_for_topo <- is(.topo, 'SpatialPointsTopography')

        if (!check_for_topo) {
          stop(".topo is not a SpatialPointsTopography object")
        }

        user_topo <- .topo
        # if the topo is provided, then we need to create the attribute of
        # offending coords, empty
        attr(user_topo, 'offending_coords') <- numeric(0)
      }

      # message("Getting the interpolator")
      # get the interpolator
      interpolator <- private$build_points_interpolator(user_dates)
      # subset the interpolator to the bbox in the sf object, this way we
      # avoid the burden of using more stations that we need, which results in
      # less time. i.e. using only the SMC stations is 20 seconds 100 points
      # 30 days, but using all stations is 170 seconds 100 points 30 days.
      interpolator_trimmed <- meteoland::subsample(
        interpolator,
        bbox = sp::bbox(user_topo),
        buffer = 100000
      )


      # default parameters
      default_params <- meteoland::defaultInterpolationParams()
      buffer_days <- max(
        default_params$St_Precipitation, default_params$St_TemperatureRange
      )

      # message("Points interpolation")
      res <- meteoland::interpolationpoints(
        object = interpolator_trimmed,
        points = user_topo,
        verbose = TRUE
      )

      # message("Naming")
      # now we need to create the names of the list res@data. Each element is
      # a point, so, we need to take the names, remove the offending coords
      # and set the names.
      points_names <- sf %>%
        dplyr::filter(
          !dplyr::row_number() %in% attr(user_topo, 'offending_coords')
        ) %>%
        dplyr::pull(!! rlang::sym(points_id))

      names(res@data) <- points_names

      return(res)
    },

    # current raster interpolation
    raster_interpolation = function(sf, user_dates) {
      # argument checks
      check_length_for(user_dates, 2, 'user_dates')
      # argument checks
      check_args_for(
        character = list(user_dates = user_dates),
        date = list(user_dates = user_dates),
        sf = list(sf = sf),
        polygons = list(sf = sf)
      )

      # This method iterate by dates to get the final rasters, as a list
      # with one element for each date supplied

      # datevec from user dates
      user_dates <- as.Date(user_dates)

      # previously to create the datevec, we must ensure end date is bigger than
      # start date
      if (! user_dates[[2]] >= user_dates[[1]]) {
        stop('end date must be equal or more recent than the start date')
      }

      datevec <-
        user_dates[[1]]:user_dates[[2]] %>%
        as.Date(format = '%j', origin = as.Date('1970-01-01'))

      sf_spatial <- sf %>%
        sf::st_transform(crs = 3043) %>%
        sf::as_Spatial()

      # safe versions of the fuctions needed
      get_lowres_raster_safe <- purrr::possibly(
        .f = self$get_lowres_raster,
        otherwise = NA
      )

      crop_safe <- purrr::possibly(
        .f = raster::crop,
        otherwise = NA
      )

      mask_safe <- purrr::possibly(
        .f = raster::mask,
        otherwise = NA
      )

      res_list <-
        datevec %>%
        as.character() %>%
        magrittr::set_names(., .) %>%
        purrr::map(~ get_lowres_raster_safe(.x, 'raster')) %>%
        purrr::map(~ crop_safe(.x, sf_spatial)) %>%
        purrr::map(~ mask_safe(.x, sf_spatial)) %>%
        purrr::keep(.p = ~ !rlang::is_na(.x))


      if (length(res_list) < 1) {
        stop("No data for the specified dates and/or polygons can be retrieved")
      }

      if (length(res_list) < length(datevec)) {

        offending_dates <-
          datevec[which(!as.character(datevec) %in% names(res_list))] %>%
          as.character() %>%
          stringr::str_flatten(collapse = ', ')

        warning(glue::glue(
          "Some dates ({offending_dates}) are not available on the database, skipping them"
        ))
      }

      return(res_list)
    },

    # get_lowres_raster method.
    # Meteoland db is a postgis db so we need to access with rpostgis and retrieve the
    # 1000x1000 raster table for the specified date
    get_lowres_raster = function(date, spatial = 'stars') {

      # argument validation
      check_args_for(
        character = list(date = date, spatial = spatial)
      )
      check_length_for(spatial, 1)
      check_if_in_for(spatial, c('stars', 'raster'))
      check_length_for(date, 1)

      # table name (it also works as cache name)
      raster_table_name <- glue::glue(
        "daily_raster_interpolated_{stringr::str_remove_all(date, '-')}"
      )

      res <- private$data_cache[[raster_table_name]] %||% {
        # pool checkout
        pool_checkout <- pool::poolCheckout(private$pool_conn)

        message(
          'Querying low res (1000x1000 meters) raster from LFC database',
          ', this can take a while...'
        )

        # get raster
        meteoland_raster <- try(
          rpostgis::pgGetRast(
            pool_checkout, raster_table_name, bands = TRUE
          )
        )
        # close checkout
        pool::poolReturn(pool_checkout)

        # check if raster inherits from try-error to stop
        if (inherits(meteoland_raster, "try-error")) {
          stop("Can not connect to the database:\n", meteoland_raster[1])
        }

        message('Done')

        # update cache
        private$data_cache[[raster_table_name]] <- meteoland_raster
        # return raster
        meteoland_raster
      }

      # now we can return a raster (just as is) or a stars object
      if (spatial == 'stars') {
        res <- res %>%
          stars::st_as_stars() %>%
          split("band")
      }

      # return the raster
      return(res)

    }

  ), # end of public methods
  # private methods
  private = list(
    # connection values
    dbname = 'meteoland',

    # point value helper methods #
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

      # here we need to check for NAs, as we need to warn the user about coords
      # out of topo. Also we remove the lines. As the most effective way, as per
      # the code, to check if topo exists is the NA in the coords_text, we
      # check that
      offending_coords_index <-
        which(is.na(raster_topography_values$coords_text))

      if (length(offending_coords_index) == length(user_coords_sp)) {
        stop("All coordinates are not in Catalonia, no interpolation available")
      }

      if (length(offending_coords_index) > 0) {
        warning(glue::glue(
          "Some points are not in Catalonia ",
          "and they they will be removed from interpolation ",
          "(indexes of offending points: {stringr::str_flatten(as.character(offending_coords_index), collapse = ', ')})",
        ))

        user_coords_sp <-
          user_coords_sp[which(!is.na(raster_topography_values$coords_text))]
        raster_topography_values <-
          raster_topography_values %>%
          dplyr::filter(!is.na(coords_text))
      }

      # build the topography object
      user_topo <- meteoland::SpatialPointsTopography(
        points = user_coords_sp,
        elevation = raster_topography_values[['elevation']],
        slope = raster_topography_values[['slope']],
        aspect = raster_topography_values[['aspect']]
      )

      # lets create an attribute with the offending coords, this way we can
      # name later the SpatialPointsMetereology@data with the identifier of the
      # geometry
      attr(user_topo, 'offending_coords') <- offending_coords_index

      return(user_topo)
    },

    # meteoland interpolator
    build_points_interpolator = function(user_dates) {

      # argument checks
      check_args_for(
        character = list(user_dates = user_dates),
        date = list(user_dates = user_dates)
      )

      # previously to create the datevec, we must ensure end date is bigger than
      # start date
      if (! user_dates[[2]] >= user_dates[[1]]) {
        stop('end date must be equal or more recent than the start date')
      }

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
        glue::glue("daily_meteo_{stringr::str_remove_all(datevec, '-')}") #%>%
        # magrittr::extract(. %in% dplyr::db_list_tables(private$pool_conn))

      # meteo data
      # TODO what happens when no table is found?????? We need to check this
      # and avoid the error, just maybe purrr::possibly or similar
      helper_station_data_getter <- function(.x) {
        dplyr::tbl(private$pool_conn, .x) %>%
          dplyr::collect() %>%
          # essential to cross results with meteo stations:
          as.data.frame() %>%
          magrittr::set_rownames(.$stationCode)
      }

      helper_station_data_getter <- purrr::possibly(
        .f = helper_station_data_getter,
        otherwise = NA
      )

      meteo_data <-
        table_names %>%
        magrittr::set_names(datevec) %>%
        purrr::map(helper_station_data_getter) %>%
        purrr::keep(.p = ~ !rlang::is_na(.x))

      if (length(meteo_data) < 1) {
        stop("No meteo data found for the dates provided")
      }

      # datevec must be trimmed to those dates really present in the database
      # in case some date was missing (see meteo_data build). Also, we need to
      # warn the user about that
      datevec_trimmed <-
        datevec[as.character(datevec) %in% names(meteo_data)]

      if (length(datevec_trimmed) != length(datevec)) {
        offending_dates <-
          datevec[which(!as.character(datevec) %in% names(meteo_data))] %>%
          as.character() %>%
          stringr::str_flatten(collapse = ', ')

        warning(glue::glue(
          "Some dates ({offending_dates}) are not available on the database, skipping them"
        ))
      }

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
        meteoland::SpatialPointsMeteorology(meteo_data, datevec_trimmed, TRUE) %>%
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
    }
  ) # end of private methods
)

## External methods ####

#' Access to the low resolution (1000x1000m) rasters in the Meteoland database
#'
#' @description \code{meteoland_get_lowres_raster} is a wrapper for the
#'   \code{$get_lowres_raster} method of \code{lfcMeteoland} objects.
#'   See also \code{\link{meteoland}}.
#'
#' @param object \code{lfcMeteoland} object, as created by
#'   \code{\link{meteoland}}
#' @param date character with the date of the raster to retrieve, i.e "2020-04-25"
#' @param spatial character vector of lenght 1 indicating the type of raster
#'   object to return, "raster" or "stars", the default.
#'
#' @return A raster object: \code{RasterBrick} if spatial is \code{raster},
#'   \code{stars} if spatial is \code{stars}. See
#'   https://r-spatial.github.io/stars/index.html for details about stars
#'   objects and \code{\link[raster]{raster}} for details about raster objects.
#'
#' @family Meteoland functions
#'
#' @details Connection to database can be slow. Rasters retrieved from the db
#'   are stored in a temporary cache inside the lfcMeteoland object created by
#'   \code{\link{meteoland}}, making subsequent calls to the same table are
#'   faster. But, be warned that in-memory rasters can use a lot of memory!
#'
#' @examples
#' if (interactive()) {
#'   meteolanddb <- meteoland()
#'   # raster
#'   ab_raster <- meteoland_get_lowres_raster(meteolanddb, '2020-04-25', 'raster')
#'   # stars
#'   ab_stars <- meteoland_get_lowres_raster(meteolanddb, '2020-04-25', 'stars')
#'
#'   # we can use pipes
#'   meteolanddb %>%
#'     meteoland_get_lowres_raster('2020-04-25', 'raster')
#'
#'   # meteolanddb is an R6 object, so the previous examples are the same as:
#'   meteolanddb$get_lowres_raster('2020-04-25', 'raster')
#'   meteolanddb$get_lowres_raster('2020-04-25', 'stars')
#' }
#'
#' @export
meteoland_get_lowres_raster <- function(object, date, spatial = 'stars') {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcMeteoland')
  # call to the class method
  object$get_lowres_raster(date, spatial)
}

#' Current points (coordinates) interpolation
#'
#' @description \code{meteoland_points_interpolation} is a wrapper for the
#'   \code{$points_interpolation} method of \code{lfcMeteoland} objects.
#'   See also \code{\link{meteoland}}.
#'
#' @param object \code{lfcMeteoland} object, as created by
#'   \code{\link{meteoland}}
#' @param sf sf object with the the point features to interpolate.
#' @param dates character vector of length 2 with the dates range (start-end) to
#'   interpolate (i.e. \code{c("2020-04-25", "2020-04-30")}). See details for
#'   more information.
#'
#' @return An SpatialPointsMetereology object (see
#'   \code{\link[meteoland]{SpatialPointsMetereology}} for more information)
#'
#' @family Meteoland functions
#'
#' @details Dates must be provided as a two elements character vector, with
#'  the start date and the end date in a format accepted by
#'  \code{\link[base]{as.Date}}.
#'  The allowed range for dates is one natural year (365 days) ending on the
#'  actual date minus one day.
#'  Interpolation for points is made based on a 30x30 meters topology grid.
#'
#'
#' @examples
#'
#' @export
meteoland_points_interpolation <- function(object, sf, dates) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcMeteoland')
  # call to the class method
  object$points_interpolation(sf, dates)
}

#' Current raster interpolation
#'
#' @description \code{meteoland_raster_interpolation} is a wrapper for the
#'   \code{$raster_interpolation} method of \code{lfcMeteoland} objects.
#'   See also \code{\link{meteoland}}.
#'
#' @param object \code{lfcMeteoland} object, as created by
#'   \code{\link{meteoland}}
#' @param sf sf object with the the polygon/multipolygons features to
#'   interpolate.
#' @param dates character vector of length 2 with the dates range (start-end) to
#'   interpolate (i.e. \code{c("2020-04-25", "2020-04-30")}). See details for
#'   more information.
#'
#' @return A list of raster objects (\code{\link[raster]{raster}}), each date
#'   as an element of that list.
#'
#' @family Meteoland functions
#'
#' @details Dates must be provided as a two elements character vector, with
#'  the start date and the end date in a format accepted by
#'  \code{\link[base]{as.Date}}.
#'  The allowed range for dates is one natural year (365 days) ending on the
#'  actual date minus one day.
#'  Interpolation for polygons is made based on a 1000x1000 meters topology
#'  raster.
#'
#' @examples
#'
#' @export
meteoland_raster_interpolation <- function(object, sf, dates) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcMeteoland')
  # call to the class method
  object$raster_interpolation(sf, dates)
}
