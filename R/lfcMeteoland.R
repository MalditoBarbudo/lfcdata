#' @description \code{meteoland()} creates an object to access the Meteoland
#'   database.
#'
#' @title lfcMeteoland class
#'
#' @return An \code{lfcMeteoland} class object (inherits from
#'   \code{\link[R6]{R6Class}}), with methods to access the data. See Methods
#'   section.
#'
#' @section Methods:
#'   \code{lfcMeteoland} objects have the following methods available:
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
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
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
    points_interpolation = function(sf, user_dates) {

      # argument checks are done in the ancillary functions, except for sf and
      # topo
      check_args_for(
        sf = list(sf = sf),
        points = list(sf = sf)
      )
      check_length_for(user_dates, 2, 'user_dates')

      # message("Getting the interpolator")
      # get the interpolator
      interpolator <- private$build_points_interpolator(user_dates)

      # get topo if needed
      if (! "elevation" %in% names(sf)) {
        user_topo <- private$get_points_topography(sf) |>
          sf::st_transform(sf::st_crs(interpolator))
      } else {
        user_topo <- sf |>
          sf::st_transform(sf::st_crs(interpolator))
        # if the topo is provided, then we need to create the attribute of
        # offending coords, empty
        attr(user_topo, 'offending_coords') <- numeric(0)
      }

      # subset the interpolator to the bbox in the sf object, this way we
      # avoid the burden of using more stations that we need, which results in
      # less time. i.e. using only the SMC stations is 20 seconds 100 points
      # 30 days, but using all stations is 170 seconds 100 points 30 days.
      buffered_user_bbox <- sf::st_bbox(sf::st_buffer(user_topo, 100000)) |>
        sf::st_as_sfc()
      stations_index <- which(sf::st_intersects(
        stars::st_get_dimension_values(interpolator, "station"), buffered_user_bbox,
        sparse = FALSE
      )[,1])

      interpolator_trimmed <- interpolator |>
        dplyr::slice(station, stations_index) |>
        meteoland::set_interpolation_params(
          meteoland::get_interpolation_params(interpolator)
        )

      # dates vec for the interpolation
      user_dates <- as.Date(user_dates)
      datevec <-
        user_dates[[1]]:user_dates[[2]] |>
        as.Date(format = '%j', origin = as.Date('1970-01-01'))

      res_interpolation <- user_topo |>
        meteoland::interpolate_data(interpolator_trimmed, dates = datevec, verbose = FALSE) |>
        tidyr::unnest(cols = "interpolated_data") |>
        dplyr::mutate(ThermalAmplitude = MaxTemperature - MinTemperature) |>
        dplyr::select(-DOY, -WindDirection)

      return(res_interpolation)
    },

    # historic points interpolation
    historical_points_interpolation = function(sf, user_dates, points_id) {
      check_args_for(
        sf = list(sf = sf),
        character = list(points_id = points_id, user_dates = user_dates),
        date = list(user_dates = user_dates),
        points = list(sf = sf)
      )
      check_length_for(user_dates, 2, 'user_dates')
      # datevec from user dates
      user_dates <- as.Date(user_dates)

      # previously to create the datevec, we must ensure end date is bigger than
      # start date
      if (! user_dates[[2]] >= user_dates[[1]]) {
        stop('end date must be equal or more recent than the start date')
      }

      datevec <-
        user_dates[[1]]:user_dates[[2]] |>
        as.Date(format = '%j', origin = as.Date('1970-01-01'))

      historical_points_interpolation_helper <- function(date, sf, points_id) {

        # check if date is historical
        if (date > Sys.Date()-366) {
          message(glue::glue("Date provided ({as.character(date)}) is not historical, but current"))
          stop(glue::glue("Date provided ({as.character(date)}) is not historical, but current"))
        }

        date_raster <- self$get_lowres_raster(as.character(date))

        date_raster |>
          stars::st_extract(sf::st_transform(sf, crs = sf::st_crs(date_raster))) |>
          dplyr::mutate(
            date = as.character(date),
            !! points_id := sf[[points_id]]
          ) |>
          dplyr::select(dplyr::all_of(c('date', points_id)), dplyr::everything())
      }

      hpih_safe <- purrr::possibly(
        .f = historical_points_interpolation_helper, otherwise = NULL
      )

      res <- datevec |>
        purrr::map(
          ~ hpih_safe(.x, sf, points_id)
        ) |>
        purrr::list_rbind()

      # checks to deliver warning or errors for missing dates or data
      if (length(unique(res[["date"]])) < length(datevec)) {

        offending_dates <- datevec[which(! as.character(datevec) %in% unique(res[["date"]]))]

        warning(glue::glue(
          "Some dates ({offending_dates}) are not available on the database, skipping them"
        ))
      }

      if (nrow(res) < 1) {
        stop("No meteo data found for any of the dates provided")
      }

      if (any(is.na(res |> dplyr::pull(MeanTemperature)))) {

        offending_points <- res |>
          dplyr::filter(is.na(MeanTemperature)) |>
          dplyr::pull(!! points_id)

        warning(glue::glue(
          "Some points are not in Catalonia ",
          "and they they will be filled with NAs ",
          "(offending points: {stringr::str_flatten(as.character(offending_points), collapse = ', ')})",
        ))
      }

      if (all(is.na(res |> dplyr::pull(MeanTemperature)))) {
        stop("All coordinates are not in Catalonia")
      }

      # return the extraction, as an sf
      res |>
        sf::st_as_sf()

    },

    # current raster interpolation, which is basically clip the polygon from the low res raster ;)
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
        user_dates[[1]]:user_dates[[2]] |>
        as.Date(format = '%j', origin = as.Date('1970-01-01')) |>
        as.character()

      raster_interpolation_helper <-
        function(date, sf) {

          stars_object <- self$get_lowres_raster(date)
          sf_transformed <- sf |>
            sf::st_transform(crs = sf::st_crs(stars_object))

          res <- stars_object |>
            sf::st_crop(sf_transformed, as_points = FALSE)

          if (all(is.na(res[["MeanTemperature"]]))) {
            stop('No data for these polygons')
          }

          return(res)
        }


      # safe versions of the functions needed
      raster_interpolation_helper_safe <- purrr::possibly(
        .f = raster_interpolation_helper, otherwise = NA
      )

      res_list <-
        datevec |>
        purrr::set_names() |>
        purrr::map(
          .f = ~ raster_interpolation_helper_safe(.x, sf)
        ) |>
        purrr::keep(.p = ~ !rlang::is_na(.x))


      if (length(res_list) < 1) {
        stop("No data for the specified dates and/or polygons can be retrieved")
      }

      if (length(res_list) < length(datevec)) {

        offending_dates <-
          datevec[which(!datevec %in% names(res_list))] |>
          as.character() |>
          stringr::str_flatten(collapse = ', ')

        warning(glue::glue(
          "Some dates ({offending_dates}) are not available on the database, skipping them"
        ))
      }

      return(res_list)
    },

    # get_lowres_raster method.
    # Meteoland db is a postgis db so we need to
    # retrieve the 1000x1000 raster table for the specified date.
    # 2023-04-21: rpostgis will be retired, so we implement a function to read rasters from
    # postgis (get_raster_from_db)
    get_lowres_raster = function(
      date, spatial = 'stars',
      # get_raster specific options
      rast_column = "rast", bands = TRUE, clip = NULL
    ) {

      # argument validation
      check_args_for(
        character = list(date = date, spatial = spatial)
      )
      check_length_for(spatial, 1)
      check_if_in_for(spatial, c('stars', 'raster'))
      check_length_for(date, 1)

      # table name (it also works as cache name)
      # The table names change when in historic period, we need to check that
      # and get the correct name
      raster_table_name <- glue::glue(
        "daily_raster_interpolated_{stringr::str_remove_all(date, '-')}"
      )

      if (as.Date(date) < Sys.Date()-365) {
        raster_table_name <- glue::glue(
          "daily_historic_raster_interpolated_{stringr::str_remove_all(date, '-')}"
        )
      }

      cache_name <- glue::glue("{raster_table_name}_{rlang::hash(bands)}{rlang::hash(clip)}")
      res <- private$data_cache[[cache_name]] %||% {

        message(
          'Querying low res (1000x1000 meters) raster from LFC database',
          ', this can take a while...'
        )

        # get raster
        meteoland_raster <- try(
          get_raster_from_db(
            private$pool_conn, raster_table_name,
            rast_column, bands, clip
          )
        )

        # check if raster inherits from try-error to stop
        if (inherits(meteoland_raster, "try-error")) {
          stop("Can not connect to the database:\n", meteoland_raster[1])
        }

        message('Done')

        # update cache
        private$data_cache[[cache_name]] <- meteoland_raster
        # return raster
        meteoland_raster
      }

      # now we can return a SpatRaster (just as is) or a stars object
      if (spatial == 'stars') {
        res <- res |>
          stars::st_as_stars()
        # only one band, stars added as attribute. More than one, stars added as
        # dimension, so we split them to attributes:
        if (rlang::is_logical(bands) || length(bands) > 1) {
          res <- res |>
            split("band")
        }
      }

      # return the raster
      return(res)

    }

  ), # end of public methods
  # private methods
  private = list(
    # connection values
    dbname = 'new_meteoland',

    # point value helper methods #
    # user topography
    get_points_topography = function(sf) {

      # argument checks
      check_args_for(
        points = list(sf = sf)
      )

      # get well known text coords
      user_coords_wkt <-
        sf |>
        sf::st_geometry() |>
        sf::st_transform(crs = 3043) |>
        sf::st_as_text(EWKT = TRUE)

      # Get db raster values
      # pool checkout
      # pool_checkout <- pool::poolCheckout(private$pool_conn)
      # SQL queries
      point_queries <-
        user_coords_wkt |>
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
            .con = private$pool_conn
          )
        )
      # return the checkout, we don't want ghost db connections
      # pool::poolReturn(pool_checkout)

      query_helper <- function(query) {
        query_res <- pool::dbGetQuery(private$pool_conn, statement = query)
        if (nrow(query_res) > 1) {
          query_res <-
            query_res |>
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
        point_queries |>
        purrr::map(query_helper) |>
        purrr::list_rbind() |>
        dplyr::as_tibble()

      # here we need to check for NAs, as we need to warn the user about coords
      # out of topo. Also we remove the lines. As the most effective way, as per
      # the code, to check if topo exists is the NA in the coords_text, we
      # check that
      offending_coords_index <-
        which(is.na(raster_topography_values$coords_text))

      if (length(offending_coords_index) == length(user_coords_wkt)) {
        stop("All coordinates are not in Catalonia, no interpolation available")
      }

      if (length(offending_coords_index) > 0) {
        warning(glue::glue(
          "Some points are not in Catalonia ",
          "and they they will be removed from interpolation ",
          "(indexes of offending points: {stringr::str_flatten(as.character(offending_coords_index), collapse = ', ')})",
        ))
      }

      filtered_topo_values <-
        raster_topography_values |>
        dplyr::filter(!is.na(coords_text)) |>
        dplyr::select(-coords_text)

      # build the topography object
      user_topo <- sf |>
        dplyr::filter(!is.na(raster_topography_values$coords_text)) |>
        dplyr::bind_cols(filtered_topo_values)

      # lets create an attribute with the offending coords
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
        (user_dates[[1]] - buffer_days):user_dates[[2]] |>
        as.Date(format = '%j', origin = as.Date('1970-01-01'))
      table_names <-
        glue::glue("daily_meteo_{stringr::str_remove_all(datevec, '-')}")

      helper_station_data_getter <- purrr::possibly(
        .f = super$get_data,
        otherwise = NA
      )

      meteo_data <-
        table_names |>
        purrr::set_names(datevec) |>
        purrr::map(helper_station_data_getter) |>
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
          datevec[which(!as.character(datevec) %in% names(meteo_data))] |>
          as.character() |>
          stringr::str_flatten(collapse = ', ')

        warning(glue::glue(
          "Some dates ({offending_dates}) are not available on the database, skipping them"
        ))
      }

      # meteo stations info
      unique_meteo_stations_data <-
        meteo_data |>
        purrr::imap(
          .f = \(meteo_df, date) {
            res <- meteo_df |>
              dplyr::mutate(dates = as.Date(date))
            # sometimes there is repeated station codes with different elevation,
            # lets get only the first station with a code
            if (length(unique(res[["stationID"]])) != nrow(res)) {
              res |>
                dplyr::group_by(stationID) |>
                dplyr::summarise_all(dplyr::first)
            }

            return(res)
          }
        ) |>
        purrr::list_rbind() |>
        dplyr::rename(stationID = stationID) |>
        # sometimes, aemet and others changes coordinates, elevation... We need to fix the coords
        # to avoid hitting a non unique stations error in meteoland::with_meteo
        dplyr::mutate(
          geometry = dplyr::last(geometry),
          elevation = dplyr::last(elevation),
          .by = stationID
        ) |>
        dplyr::mutate(
          geometry = geometry |>
            rlang::parse_exprs() |>
            purrr::map(eval) |>
            purrr::map(sf::st_point) |>
            sf::st_as_sfc(crs = 4326)
        ) |>
        # finally convert to sf and set the CRS
        sf::st_as_sf()

      # Before returning the interpolator, we need to change some params based
      # on the latest calibrations
      latest_calibration <-
        super$get_data('interpolation_parameters') |>
        dplyr::filter(year == max(year, na.rm = TRUE)) |>
        dplyr::collect()

      # create the interpolator
      interpolator_res <- meteoland::with_meteo(unique_meteo_stations_data, verbose = FALSE) |>
        meteoland::create_meteo_interpolator(
          params = list(
            N_MinTemperature = latest_calibration$N_MinTemperature,
            alpha_MinTemperature = latest_calibration$alpha_MinTemperature,
            N_MaxTemperature = latest_calibration$N_MaxTemperature,
            alpha_MaxTemperature = latest_calibration$alpha_MaxTemperature,
            N_DewTemperature = latest_calibration$N_DewTemperature,
            alpha_DewTemperature = latest_calibration$alpha_DewTemperature,
            N_PrecipitationEvent = latest_calibration$N_PrecipitationEvent,
            alpha_PrecipitationEvent = latest_calibration$alpha_PrecipitationEvent,
            N_PrecipitationAmount = latest_calibration$N_PrecipitationAmount,
            alpha_PrecipitationAmount = latest_calibration$alpha_PrecipitationAmount
          ),
          verbose = FALSE
        )

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
#'   meteolanddb |>
#'     meteoland_get_lowres_raster('2020-04-25', 'raster')
#'
#'   # meteolanddb is an R6 object, so the previous examples are the same as:
#'   meteolanddb$get_lowres_raster('2020-04-25', 'raster')
#'   meteolanddb$get_lowres_raster('2020-04-25', 'stars')
#' }
#'
#' @export
meteoland_get_lowres_raster <- function(object, date, spatial = 'stars', rast_column = "rast", bands = TRUE, clip = NULL) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcMeteoland')
  # call to the class method
  object$get_lowres_raster(date, spatial, rast_column, bands, clip)
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
#' @param .topo optional custom SpatialPointsTopology object. If not provided
#'   topology will be retrieved from database (only for Catalonia)
#'
#' @return An sf object if \code{.as_sf} is TRUE (default), an
#'   SpatialPointsMetereology object (see
#'   \code{\link[meteoland]{SpatialPointsMetereology}} for more information) if
#'   \code{.as_sf} is FALSE.
#'
#' @family Meteoland functions
#'
#' @details Dates must be provided as a two elements character vector, with
#'  the start date and the end date in a format accepted by
#'  \code{\link[base]{as.Date}}.
#'  The allowed range for dates is one natural year (365 days) ending on the
#'  actual date minus one day.
#'  Interpolation for points is made based on a 30x30 meters topology grid.#'
#'
#' @examples
#' if (interactive()) {
#'   library(lfcdata)
#'   meteolanddb <- meteoland()
#'   sf_points <- nfi()$get_data('plots', spatial = TRUE) |>
#'   dplyr::slice(1:5) |>
#'   dplyr::select(plot_id)
#'
#'   meteoland_points_interpolation(
#'     meteolanddb, sf_points, c(Sys.Date()-1, Sys.Date()-2)
#'   )
#' }
#'
#'
#' @export
meteoland_points_interpolation <- function(
    object, sf, dates
) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcMeteoland')
  # call to the class method
  object$points_interpolation(sf, dates)
}

#' Historical points (coordinates) interpolation
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
#' @return An sf object.
#'
#' @family Meteoland functions
#'
#' @details Dates must be provided as a two elements character vector, with
#'  the start date and the end date in a format accepted by
#'  \code{\link[base]{as.Date}}.
#'  The allowed range for dates is one natural year (365 days) ending on the
#'  actual date minus one day.
#'  Interpolation for points is made based on a 30x30 meters topology grid.#'
#'
#' @examples
#' if (interactive()) {
#'   library(lfcdata)
#'   meteolanddb <- meteoland()
#'   sf_points <- nfi()$get_data('plots', spatial = TRUE) |>
#'   dplyr::slice(1:5) |>
#'   dplyr::select(plot_id)
#'
#'   meteoland_historical_points_interpolation(
#'     meteolanddb, sf_points, c(Sys.Date()-1, Sys.Date()-2, "plot_id")
#'   )
#' }
#'
#'
#' @export
meteoland_historical_points_interpolation <- function(
    object, sf, dates, points_id
) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcMeteoland')
  # call to the class method
  object$historical_points_interpolation(sf, dates, points_id)
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
#' @export
meteoland_raster_interpolation <- function(object, sf, dates) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcMeteoland')
  # call to the class method
  object$raster_interpolation(sf, dates)
}
