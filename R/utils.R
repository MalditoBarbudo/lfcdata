# syntactic sugar for "one or another if one is null" case. taken from tidyverse utils
`%||%` <- function(a, b) {
  if (rlang::is_null(a)) b else a
}

# get data "generic"
.lfcproto_get_data <- function(private, table_name, spatial) {

  # try to catch a db connection error
  query_data <- try(
    dplyr::tbl(private$pool_conn, table_name) %>% dplyr::collect()
  )
  if (inherits(query_data, "try-error")) {
    stop("Can not connect to the database:\n", query_data[1])
  }

  # is the query spatial?
  if (!spatial) {
    # if not,return the data
    return(query_data)
  } else {
    # if it is, then convert based on the lat and long vars
    if (all(c('coords_longitude', 'coords_latitude') %in% names(query_data))) {
      query_data_spatial <- query_data %>%
        sf::st_as_sf(
          coords = c('coords_longitude', 'coords_latitude'), remove = FALSE,
          crs = 4326
        )
    } else {
      # if there is no lat long vars, then get them from plots
      query_data_spatial <- query_data %>%
        dplyr::left_join(
          dplyr::tbl(private$pool_conn, 'plots') %>%
            dplyr::select(plot_id, coords_longitude, coords_latitude) %>%
            dplyr::collect(),
          by = 'plot_id'
        ) %>%
        sf::st_as_sf(
          coords = c('coords_longitude', 'coords_latitude'), remove = FALSE,
          crs = 4326
        )
    }
    return(query_data_spatial)
  }

}
