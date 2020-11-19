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
#'     \item{\code{$get_raster}: Returns the raster for the selected date
#'     and selected resolution}
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
          " to obtain a raster for the desired date and resolution.\n",
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
      date, resolution, spatial = 'stars'
    ) {
      # argument validation
      check_args_for(
        character = list(date = date, spatial = spatial, resolution = resolution)
      )
      check_length_for(spatial, 1)
      check_length_for(resolution, 1)
      check_length_for(date, 1)
      check_if_in_for(spatial, c('stars', 'raster'))
      check_if_in_for(resolution, c('1km', '200m', 'smoothed'))

      date_parsed <- stringr::str_remove_all(date, pattern = '-')

      resolution_parsed <- switch(
        resolution,
        'smoothed' = 'smooth',
        '1km' = 'low',
        '200m' = 'high'
      )

      raster_table_name <- glue::glue(
        "catdrought_{resolution_parsed}_{date_parsed}"
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

        ## Set the correct names on the layers. I don't know why but when
        ## building the database, when the temp table is copied to the
        ## partitioned table, layer names are lost
        names(catdrought_raster) <- c(
          'DDS', 'DeepDrainage', 'Eplant', 'Esoil', 'Infiltration',
          'LAI', 'PET', 'Psi', 'REW', 'Runoff', 'Theta'
        )

        # if there is an error, stop
        if (inherits(catdrought_raster, "try-error")) {
          stop("Can not connect to the database")
        }

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

    get_current_time_series = function(sf, resolution) {

      # argument check
      check_args_for(
        sf = list(sf = sf),
        character = list(resolution = resolution)
      )
      check_length_for(resolution, 1)
      check_if_in_for(resolution, c('1km', '200m', 'smoothed'))

      # we will need the table name based on the desired resolution
      # now the table name
      table_name <- glue::glue(
        "daily.catdrought_{resolution}"
      )

      browser()

      # look which kind of sf is, points or polygons
      # POLYGONS
      if (all(sf::st_is(sf, type = c('POLYGON', 'MULTIPOLYGON')))) {

        # we need the sf as text to create the SQL query
        sf_text <- sf %>%
          # first thing here is to transform to the correct coordinates system
          sf::st_transform(crs = 4326) %>%
          # convert to text
          dplyr::pull(geometry) %>%
          sf::st_as_text()

        # Now we build the query and get the polygon/s values
        # data query to get the dump of the data
        data_query <- glue::glue(
          "
          with
          feat as (select st_geomfromtext('{sf_text}', 4326) as geom),
          b_stats as (select day, (stats).* from (select day, ST_SummaryStats(st_clip(rast, 1, geom, true)) as stats
            from {table_name}
            inner join feat
            on st_intersects(feat.geom,rast)
          ) as foo
          )
          select
            day,
            sum(mean*count)/sum(count) as avg_pval,
            sqrt(sum(stddev*stddev*count)/sum(count)) as se_pval
          from b_stats
          where count > 0
          group by day;
        "
        )

        res <- pool::dbGetQuery(private$pool_conn, data_query)




      }



    }
  ),

  # private methods
  private = list(
    # connection values
    dbname = 'catdrought_db'
  )


)
