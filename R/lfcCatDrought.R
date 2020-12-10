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
        crayon::blue$underline("laboratoriforestal.creaf.uab.cat\n\n"),
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

    get_raster = function(
      date, spatial = 'stars'
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

      res <- private$data_cache[[raster_table_name]] %||% {
        # pool checkout
        pool_checkout <- pool::poolCheckout(private$pool_conn)

        message(
          "Querying raster from LFC database, ",
          "this can take a while..."
        )

        # try to get the raster
        catdrought_raster <- try({
          rpostgis::pgGetRast(
            pool_checkout, name = c('daily', raster_table_name), bands = TRUE
          )
        })
        pool::poolReturn(pool_checkout)

        # if there is an error, stop
        if (
          inherits(catdrought_raster, "try-error") & stringr::str_detect(
            catdrought_raster, "raster column 'rast' not found"
          )
        ) {
          stop(glue::glue("Selected date {date} is not available in the database"))
        }

        ## Set the correct names on the layers. I don't know why but when
        ## building the database, when the temp table is copied to the
        ## partitioned table, layer names are lost
        names(catdrought_raster) <- c(
          'DDS', 'DeepDrainage', 'Eplant', 'Esoil', 'Infiltration',
          'LAI', 'PET', 'Psi', 'REW', 'Runoff', 'Theta'
        )

        message("Done")

        # update cache
        private$data_cache[[raster_table_name]] <- catdrought_raster
        # return the raster
        catdrought_raster
      }

      if (spatial == 'stars') {
        res <- res %>%
          stars::st_as_stars() %>%
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
          'LAI', 'PET', 'Psi', 'REW', 'Runoff', 'Theta'
        )
      )

      # switches
      band <- switch(
        variable,
        'DDS' = 1,
        'DeepDrainage' = 2,
        'Eplant' = 3,
        'Esoil' = 4,
        'Infiltration' = 5,
        'LAI' = 6,
        'PET' = 7,
        'Psi' = 8,
        'REW' = 9,
        'Runoff' = 10,
        'Theta' = 11
      )

      # now the table name
      table_name <- "catdrought_low"

      # transform the sf to the coord system in the db
      sf_transf <- sf %>%
        # first thing here is to transform to the correct coordinates system
        sf::st_transform(crs = 4326)

      # we need also the identifiers of the polygons
      sf_id <- sf_transf %>%
        dplyr::pull(1)

      # get the correct geometry column
      sf_column <- attr(sf_transf, 'sf_column')

      # we need the sf as text to create the SQL query
      sf_text <- sf_transf %>%
        # convert to text
        dplyr::pull({{sf_column}}) %>%
        sf::st_as_text()

      # look which kind of sf is, points or polygons
      # POLYGONS
      if (all(sf::st_is(sf, type = c('POLYGON', 'MULTIPOLYGON')))) {

        # Now we build the query and get the polygon/s values
        # data query to get the dump of the data
        pool_checkout <- pool::poolCheckout(private$pool_conn)
        data_queries <- glue::glue_sql(
          "
          with
          feat as (select st_geomfromtext({sf_text}, 4326) as geom),
          b_stats as (select day, (stats).* from (select day, ST_SummaryStats(st_clip(rast, {band}, geom, true)) as stats
            from daily.{`table_name`}
            inner join feat
            on st_intersects(feat.geom,rast)
          ) as foo
          )
          select
            day,
            sum(mean*count)/sum(count) as mean
          from b_stats
          where count > 0
          group by day;
        ", .con = pool_checkout
        ) %>%
          magrittr::set_names(sf_id)
        # now the quantiles
        quantile_queries <- glue::glue_sql(
          "
          SELECT day, (ST_Quantile(rast,{band},ARRAY[0.9,0.1])).* As pvc
          FROM daily.{`table_name`}
          WHERE ST_Intersects(rast,
            ST_GeomFromText({sf_text}, 4326)
          )
          ORDER BY day,quantile,value
          ;
          ", .con = pool_checkout
        ) %>%
          magrittr::set_names(sf_id)
        pool::poolReturn(pool_checkout)

        tictoc::tic()
        res_stats <-
          data_queries %>%
          purrr::imap_dfr(
            ~ pool::dbGetQuery(private$pool_conn, .x) %>%
              dplyr::mutate(polygon_id = .y)
          ) %>%
          dplyr::arrange(day, polygon_id) %>%
          dplyr::select(day, polygon_id, mean)
        res_quantiles <-
          quantile_queries %>%
          purrr::imap_dfr(
            ~ pool::dbGetQuery(private$pool_conn, .x) %>%
              dplyr::mutate(polygon_id = .y)
          ) %>%
          tidyr::pivot_wider(
            names_from = quantile,
            names_glue = 'q_{quantile*100}',
            values_from = value
          )

        res <- res_stats %>%
          dplyr::left_join(res_quantiles) %>%
          dplyr::as_tibble()
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
        pool_checkout <- pool::poolCheckout(private$pool_conn)
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
        ", .con = pool_checkout
        ) %>%
          magrittr::set_names(sf_id)
        pool::poolReturn(pool_checkout)

        tictoc::tic()
        res <-
          data_queries %>%
          purrr::imap_dfr(
            ~ pool::dbGetQuery(private$pool_conn, .x) %>%
              dplyr::mutate(point_id = .y)
          ) %>%
          dplyr::arrange(day, point_id) %>%
          dplyr::select(day, point_id, "{variable}" := pixel_value)
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
#'   catdroughtdb %>%
#'     catdrought_get_raster('2020-04-25', 'smoothed', 'raster')
#'
#'   # catdroughtdb is an R6 object, so the previous examples are the same as:
#'   catdroughtdb$get_raster('2020-04-25', 'smoothed', 'raster')
#'   catdroughtdb$get_raster('2020-04-25', 'smoothed', 'stars')
#' }
#'
#' @export
catdrought_get_raster <- function(object, date, spatial = 'stars') {
  # argument validation
  # NOTE: variables and spatial are validated in the method
  check_class_for(object, 'lfcCatDrought')
  # call to the class method
  object$get_raster(date, spatial)
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
