#' @description \code{catdrought()} creates an object to access the CatDrought
#'   database.
#'
#' @title lfcCatDrought class
#'
#' @return An \code{lfcCatDrought} class object (inherits from
#'   \code{\link[R6]{R6Class}}), with methods to access the data. See Methods
#'   section.
#'
#' @section Methods:
#'   \code{lfcCatDrought} objects have the following methods available:
#'   \itemize{
#'     \item{\code{$get_data}: Returns none, maintaned for consistency}
#'     \item{\code{$get_raster}: Returns the raster for the selected date}
#'     \item{\code{$get_current_time_series}: Returns a dataframe with the
#'     time series for a provided spatial object (points or polygons)}
#'   }
#'
#' @family catdrought functions
#'
#' @export
#'
#' @examples
#' catdroughtdb <- catdrought()
#' catdroughtdb
catdrought <- function() {
  lfcCatDrought$new()
}

## lfcCatDrought Class ####
lfcCatDrought <- R6::R6Class(
  # specs
  classname = 'lfcCatDrought',
  inherit = lfcObject,
  cloneable = FALSE,

  # public methods
  public = list(
    # override the defailt print
    print = function(...) {
      cat(
        "Access to the CatDrought database.\n",
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
        "Use " %+% crayon::yellow$bold("catdrought_get_raster") %+%
          " to obtain a raster for the desired date.\n",
        "Use " %+% crayon::yellow$bold("catdrought_current_time_series") %+%
          " to obtain a dataframe with the current natural year time series.\n",
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

    describe_var = function(variables) {
      # argument checks
      check_args_for(character = list(variables = variables))
      check_if_in_for(
        variables,
        c(
          'DDS', 'DeepDrainage', 'Eplant', 'Esoil', 'Infiltration',
          'Interception', 'LAI', 'LMFC', 'PET', 'Precipitation', 'Psi', 'REW',
          'Runoff', 'Theta'
        )
      )

      # cats
      catdrought_describe_var_cat(variables)

      # as the print method, to allow $ piping
      return(invisible(self))
    },

    get_raster = function(
      date, spatial = 'stars', rast_column = "rast", bands = TRUE, clip = NULL
    ) {
      # argument validation
      check_args_for(
        character = list(date = date, spatial = spatial)
      )
      check_length_for(spatial, 1)
      check_length_for(date, 1)
      check_if_in_for(spatial, c('stars', 'raster'))

      date_parsed <- stringr::str_remove_all(date, pattern = '-')

      raster_table_name <- glue::glue(
        "catdrought_low_{date_parsed}"
      )

      cache_name <- glue::glue("{raster_table_name}_{rlang::hash(bands)}{rlang::hash(clip)}")

      res <- private$data_cache$get(cache_name)
      if (cachem::is.key_missing(res)) {
        message(
          "Querying raster from LFC database, ",
          "this can take a while..."
        )

        # get the fixed table name (as catdrought uses the daily schema)
        schema <- "daily"
        raster_table_name <- glue::glue_sql("{`schema`}.{`raster_table_name`}", .con = private$pool_conn)

        # try to get the raster
        catdrought_raster <- try({
          get_raster_from_db(
            private$pool_conn,
            # c
            raster_table_name,
            rast_column, bands, clip
          )
        })

        # if there is an error, stop
        if (
          inherits(catdrought_raster, "try-error") && stringr::str_detect(
            catdrought_raster, "table missing"
          )
        ) {
          stop(glue::glue("Selected date {date} is not available in the database"))
        }

        ## Set the correct names on the layers. I don't know why but when
        ## building the database, when the temp table is copied to the
        ## partitioned table, layer names are lost
        names(catdrought_raster) <- c(
          'DDS', 'DeepDrainage', 'Eplant', 'Esoil', 'Infiltration',
          'Interception', 'LAI', 'LMFC', 'PET', 'Precipitation', 'Psi', 'REW',
          'Runoff', 'Theta'
        )[bands]

        message("Done")

        # update cache
        private$data_cache$set(cache_name, catdrought_raster)
        # return the raster
        res <- catdrought_raster
      }
      
      # res <- private$data_cache[[cache_name]] %||% {
      #   # pool checkout
      #   # pool_checkout <- pool::poolCheckout(private$pool_conn)

      #   message(
      #     "Querying raster from LFC database, ",
      #     "this can take a while..."
      #   )

      #   # get the fixed table name (as catdrought uses the daily schema)
      #   schema <- "daily"
      #   raster_table_name <- glue::glue_sql("{`schema`}.{`raster_table_name`}", .con = private$pool_conn)

      #   # try to get the raster
      #   catdrought_raster <- try({
      #     get_raster_from_db(
      #       private$pool_conn,
      #       # c
      #       raster_table_name,
      #       rast_column, bands, clip
      #     )
      #   })
      #   # catdrought_raster <- try({
      #   #   rpostgis::pgGetRast(
      #   #     pool_checkout, name = c('daily', raster_table_name), bands = TRUE
      #   #   )
      #   # })
      #   # pool::poolReturn(pool_checkout)

      #   # if there is an error, stop
      #   if (
      #     inherits(catdrought_raster, "try-error") && stringr::str_detect(
      #       catdrought_raster, "table missing"
      #     )
      #   ) {
      #     stop(glue::glue("Selected date {date} is not available in the database"))
      #   }

      #   ## Set the correct names on the layers. I don't know why but when
      #   ## building the database, when the temp table is copied to the
      #   ## partitioned table, layer names are lost
      #   names(catdrought_raster) <- c(
      #     'DDS', 'DeepDrainage', 'Eplant', 'Esoil', 'Infiltration',
      #     'Interception', 'LAI', 'LMFC', 'PET', 'Precipitation', 'Psi', 'REW',
      #     'Runoff', 'Theta'
      #   )[bands]

      #   message("Done")

      #   # update cache
      #   private$data_cache[[cache_name]] <- catdrought_raster
      #   # return the raster
      #   catdrought_raster
      # }

      if (spatial == 'stars') {
        res <- res |>
          stars::st_as_stars() |>
          split('band')
      }

      return(res)

    },

    get_current_time_series = function(sf, variable) {

      # argument check
      check_args_for(
        sf = list(sf = sf),
        character = list(variable = variable)
      )
      check_length_for(variable, 1)
      check_if_in_for(
        variable, c(
          'DDS', 'DeepDrainage', 'Eplant', 'Esoil', 'Infiltration',
          'Interception', 'LAI', 'LMFC', 'PET', 'Precipitation', 'Psi', 'REW',
          'Runoff', 'Theta'
        )
      )

      # switches
      band <- switch(
        variable,
        'DDS' = 1L,
        'DeepDrainage' = 2L,
        'Eplant' = 3L,
        'Esoil' = 4L,
        'Infiltration' = 5L,
        'Interception' = 6L,
        'LAI' = 7L,
        'LMFC' = 8L,
        'PET' = 9L,
        'Precipitation' = 10L,
        'Psi' = 11L,
        'REW' = 12L,
        'Runoff' = 13L,
        'Theta' = 14L
      )

      # now the table name
      table_name <- "catdrought_low"

      # transform the sf to the coord system in the db
      sf_transf <- sf |>
        # first thing here is to transform to the correct coordinates system
        sf::st_transform(crs = 4326)

      # we need also the identifiers of the polygons
      sf_id <- sf_transf |>
        dplyr::pull(1)

      # get the correct geometry column
      sf_column <- attr(sf_transf, 'sf_column')

      # we need the sf as text to create the SQL query
      sf_text <- sf_transf |>
        # convert to text
        dplyr::pull({{sf_column}}) |>
        sf::st_as_text()

      # look which kind of sf is, points or polygons
      # POLYGONS
      if (all(sf::st_is(sf, type = c('POLYGON', 'MULTIPOLYGON')))) {

        # Now we build the query and get the polygon/s values
        # data query to get the dump of the data
        # pool_checkout <- pool::poolCheckout(private$pool_conn)
        data_queries <- glue::glue_sql(
          "SELECT
           day,
           (ST_SummaryStatsAgg(ST_Clip(rast,geom, -9999, true),{band},true)).*
         FROM
           daily.{`table_name`},
           (select st_geomfromtext({sf_text}, 4326) as geom) AS feat
         WHERE
           ST_Intersects(rast, geom)
         GROUP BY day;", .con = private$pool_conn
        ) |>
          purrr::set_names(sf_id)

        # pool::poolReturn(pool_checkout)

        dates_available <- seq(
          lubridate::ymd(Sys.Date() - 366), lubridate::ymd(Sys.Date() - 1),
          # lubridate::ymd(Sys.Date() - 366), lubridate::ymd('2020-12-12'),
          by = 'days'
        )

        tictoc::tic()
        res <-
          data_queries |>
          purrr::imap_dfr(
            ~ pool::dbGetQuery(private$pool_conn, .x) |>
              dplyr::mutate(polygon_id = .y)
          ) |>
          dplyr::arrange(day, polygon_id) |>
          dplyr::select(day, polygon_id, dplyr::everything()) |>
          dplyr::mutate(
            stderror = stddev/sqrt(count)
          ) |>
          dplyr::as_tibble() |>
          dplyr::filter(day %in% dates_available)
        tictoc::toc()

        # res checks for warnings or errors
        if (nrow(res) < 1) {
          stop("All polygons are out of bounds of the raster")
        }

        if (length(sf_id) > length(unique(res[['polygon_id']]))) {
          warning("One or more polygons are out of bounds of the raster and were removed")
        }

        return(res)
      }

      if (all(sf::st_is(sf, type = c('POINT', 'MULTIPOINT')))) {
        # Now we build the query and get the polygon/s values
        # data query to get the dump of the data
        # pool_checkout <- pool::poolCheckout(private$pool_conn)
        data_queries <- glue::glue_sql(
          "
          SELECT day, ST_Value(
            rast,
            {band},
            st_geomfromtext({sf_text}, 4326)
          ) As pixel_value
          FROM daily.{`table_name`}
          WHERE ST_Intersects(
            rast,
            st_geomfromtext({sf_text}, 4326)
          )
        ", .con = private$pool_conn
        ) |>
          purrr::set_names(sf_id)
        # pool::poolReturn(pool_checkout)

        dates_available <- seq(
          lubridate::ymd(Sys.Date() - 366), lubridate::ymd(Sys.Date() - 1),
          # lubridate::ymd(Sys.Date() - 366), lubridate::ymd('2020-12-12'),
          by = 'days'
        )

        tictoc::tic()
        res <-
          data_queries |>
          purrr::imap_dfr(
            ~ pool::dbGetQuery(private$pool_conn, .x) |>
              dplyr::mutate(point_id = .y)
          ) |>
          dplyr::arrange(day, point_id) |>
          dplyr::select(day, point_id, "{variable}" := pixel_value) |>
          dplyr::filter(day %in% dates_available)
        tictoc::toc()

        # res checks for warnings or errors
        if (nrow(res) < 1) {
          stop("All points are out of bounds of the raster")
        }

        if (length(sf_id) > length(unique(res[['point_id']]))) {
          warning("One or more points are out of bounds of the raster and were removed")
        }

        return(res)
      }



    }
  ),

  # private methods
  private = list(
    # connection values
    dbname = 'catdrought_db'
  )


)


## External methods ####

#' Access to the rasters in the Catdrought database
#'
#' @description \code{catdrought_get_raster} is a wrapper for the
#'   \code{$get_raster} method of \code{lfcCatDrought} objects.
#'   See also \code{\link{catdrought}}.
#'
#' @param object \code{lfcCatDrought} object, as created by
#'   \code{\link{catdrought}}
#' @param date character with the date of the raster to retrieve, i.e "2020-04-25"
#' @param spatial character vector of length 1 indicating the type of raster
#'   object to return, "raster" or "stars", the default.
#'
#' @return A raster object: \code{RasterBrick} if spatial is \code{raster},
#'   \code{stars} if spatial is \code{stars}. See
#'   https://r-spatial.github.io/stars/index.html for details about stars
#'   objects and \code{\link[raster]{raster}} for details about raster objects.
#'
#' @family catdrought functions
#'
#' @details Connection to database can be slow. Rasters retrieved from the db
#'   are stored in a temporary cache inside the lfcCatDrought object created by
#'   \code{\link{catdrought}}, making subsequent calls to the same table are
#'   faster. But, be warned that in-memory rasters can use a lot of memory!
#'
#' @examples
#' if (interactive()) {
#'   catdroughtdb <- catdrought()
#'   # raster
#'   catdrougth_20200425_smoothed <-
#'     catdrought_get_raster(catdroughtdb, '2020-04-25', 'smoothed', 'raster')
#'   # stars
#'   catdrougth_20200425_smoothed_stars <-
#'     catdrought_get_raster(catdroughtdb, '2020-04-25', 'smoothed', 'stars')
#'
#'   # we can use pipes
#'   catdroughtdb |>
#'     catdrought_get_raster('2020-04-25', 'smoothed', 'raster')
#'
#'   # catdroughtdb is an R6 object, so the previous examples are the same as:
#'   catdroughtdb$get_raster('2020-04-25', 'smoothed', 'raster')
#'   catdroughtdb$get_raster('2020-04-25', 'smoothed', 'stars')
#' }
#'
#' @export
catdrought_get_raster <- function(object, date, spatial = 'stars', rast_column = "rast", bands = TRUE, clip = NULL) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcCatDrought')
  # call to the class method
  object$get_raster(date, spatial, rast_column, bands, clip)
}

#' Create time series for CatDrought variables for the current year
#'
#' @description \code{catdrought_get_time_series} is a wrapper for the
#'   \code{$get_current_time_series} method of \code{lfcCatDrought} objects.
#'   See also \code{\link{catdrought}}.
#'
#' @param object \code{lfcCatDrought} object, as created by
#'   \code{\link{catdrought}}
#' @param sf sf object with polygons or points where to calculate the time
#'   series
#' @param variable character indicating the desired variable to create the
#'   time series. It should be one of 'DDS', 'DeepDrainage', 'Eplant', 'Esoil',
#'   'Infiltration', 'LAI', 'PET', 'Psi', 'REW', 'Runoff' or 'Theta'
#'
#' @return A data frame with the date, sf identification and the
#' variable value for points or the mean and standard error values for polygons.
#'
#' @family catdrought functions
#'
#' @details Calculations can be long depending on the number of features and/or
#'   size of polygons.
#'
#' @section sf:
#'   sf objects must have a column with unique values for each feature as an
#'   identifier. This must be the first column in the sf object.
#'
#' @examples
#' if (interactive()) {
#'   # TODO examples
#' }
#'
#' @export
catdrought_get_current_time_series <- function(object, sf, variable) {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcCatDrought')
  # call to the class method
  object$get_current_time_series(sf, variable)
}

#' Print info about the variables present in the CatDrought db
#'
#' @description \code{catdrought_describe_var} is a wrapper for the \code{$describe_var} method
#'   of \code{lfcCatDrought} objects. See \code{\link{catdrought}}.
#'
#' @param object \code{lfcCatDrought} object, as created by \code{\link{catdrought}}
#' @param variables character vector with the names of the variables to describe
#'
#' @return A character vector with the variable names to describe
#'
#' @family catdrought functions
#'
#' @examples
#' if (interactive()) {
#' catdroughtdb <- catdrought()
#' catdrought_describe_var(catdroughtdb, "DDS")
#' catdrought_describe_var(catdroughtdb, c("Esoil", "REW"))
#'
#' # catdroughtdb is an R6 object, so the previous example is the same as:
#' catdroughtdb$describe_var("DDS")
#' catdroughtdb$describe_var(c("Esoil", "REW"))
#' }
#'
#' @export
catdrought_describe_var <- function(object, variables) {
  # argument validation
  check_class_for(object, 'lfcCatDrought')
  # call to the class method
  object$describe_var(variables)
}
