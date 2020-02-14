cochrane_sd_reduce <- function(n, m, s) {

  res_vec <- seq_along(n) %>%
    purrr::map(~ c(n = n[.x], m = m[.x], s = s[.x])) %>%
    purrr::reduce(
      .f = function(first, second) {
        stddev <- sqrt(
          # numerador
          (((first['n'] - 1)*(first['s']^2)) + ((second['n'] - 1)*(second['s']^2)) +
             (((first['n']*second['n'])/(first['n'] + second['n']))*(first['m']^2 + second['m']^2 - (2*first['m']*second['m'])))) /
            # denominador
            (first['n'] + second['n'] - 1)
        ) %>% unname()
        count <- sum(first['n'], second['n'])
        mean_temp <- (((first['n']*first['m']) + (second['n']*second['m']))/count) %>% unname()

        return(c(n = count, m = mean_temp, s = stddev))
      }
    )

  return(res_vec['s'])
}

# clip and mean
clip_and_mean_simple_case <- function(sf, poly_id, var_name, lidardb) {

  cat(
    crayon::yellow$bold(glue::glue("Processing {poly_id} polygon..."))
  )

  # poly as wkt, to avoid table creation
  wkt_poly <-
    sf %>%
    sf::st_geometry() %>%
    sf::st_as_text(EWKT = TRUE)

  # feature query. In this query we create the simple feature table-like
  feat_query <- glue::glue_sql(
    "SELECT {poly_id} As poly_id, ST_GeomFromEWKT({wkt_poly}) As geometry",
    .con = lidardb$.__enclos_env__$private$pool_conn
  )

  # stats query. In this query, IIUC, we join the raster to the geature on the tiles
  # intersecting, and we calculate the summary stats for the tiles. We return this, as
  # in this way we can calculate not on ly the mean, but also the std deviation.
  b_stats_query <- glue::glue_sql(
    "SELECT poly_id, geometry, (ST_SummaryStats(ST_Clip(rast,1,geometry, true),1,true)).*
         FROM {`var_name`}
       INNER JOIN ({feat_query}) AS feat
       ON ST_Intersects(feat.geometry,rast)",
    .con = lidardb$.__enclos_env__$private$pool_conn
  )

  # execute the query and retrieve the data
  intersecting_tiles_stats <- sf::st_read(
    lidardb$.__enclos_env__$private$pool_conn, query = b_stats_query, as_tibble = TRUE
  )

  polygon_stats <-
    intersecting_tiles_stats %>%
    dplyr::as_tibble() %>%
    dplyr::filter(count > 0) %>%
    dplyr::group_by(poly_id) %>%
    dplyr::mutate(count = as.integer(count)) %>%
    dplyr::summarise(
      pixels = sum(.data[['count']]),
      average = sum(.data[['count']]*.data[['mean']])/sum(.data[['count']]),
      min = min(.data[['min']]),
      max = max(.data[['max']]),
      sd = cochrane_sd_reduce(n = .data[['count']], m = .data[['mean']], s = .data[['stddev']])
    )

  cat(
    crayon::yellow$bold(glue::glue(" done.")), '\n'
  )

  return(polygon_stats)
}

clip_and_mean_vectorized_for_polys <- function(data, id_var_name, var_name, lidardb) {
  # get the geom column name
  sf_column <- attr(data, 'sf_column')

  summ_polys_data <-
    seq_along(data[[sf_column]]) %>%
    purrr::map_dfr(
      ~ clip_and_mean_simple_case(
        sf = data[[sf_column]][.x],
        poly_id = data[[id_var_name]][.x],
        var_name = var_name,
        lidardb = lidardb
      )
    )

  return(summ_polys_data)
}


supposedly_good_results <- sf::read_sf(
  lidardb$.__enclos_env__$private$pool_conn, 'lidar_municipios'
) %>%
  dplyr::select(poly_id, mean_dbh)

library(tictoc)
tic()
clip_and_mean_vectorized_for_polys(
  dplyr::slice(supposedly_good_results, 1:50), 'poly_id', 'dbh', lidardb
)
toc()
sum(dplyr::slice(supposedly_good_results, 1:50) %>% sf::st_area())/1000000

tic()
clip_and_mean_vectorized_for_polys(
  supposedly_good_results, 'poly_id', 'dbh', lidardb
)
toc()
sum(supposedly_good_results %>% sf::st_area())/1000000

provincias_test <- sf::read_sf(
  lidardb$.__enclos_env__$private$pool_conn, 'lidar_provincias'
) %>%
  dplyr::select(poly_id, mean_dbh)
tic()
clip_and_mean_vectorized_for_polys(
  provincias_test, 'poly_id', 'dbh', lidardb
)
toc()

sum(provincias_test %>% sf::st_area())/1000000
