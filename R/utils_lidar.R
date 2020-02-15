# Combine statistics for different groups. ST_SummaryStats in postgis SQL returns the
# statistics for each tile the polygon intersects. This means that calcualting the n and
# the mean is easy, but we need to combine the statistics to calculate the standard
# deviation of the entire sample.
# We use the Cochrane Reviews formulae:
# https://handbook-5-1.cochrane.org/front_page.htm
# https://handbook-5-1.cochrane.org/chapter_7/table_7_7_a_formulae_for_combining_groups.htm
cochrane_sd_reduce <- function(n, m, s) {

  res_vec <- seq_along(n) %>%
    # we need to work with vectors of c(n1,m1,s1); c(n2,m2,s2)...
    purrr::map(~ c(n = n[.x], m = m[.x], s = s[.x])) %>%
    # and we reduce those vectors to create a new vector with the groups combined stats
    # to use with the following vector:
    #
    # at start:         list(c(n1,m1,s1), c(n2,m2,s2), ..., c(nn,mn,sn))
    # after first step: list(c(nc1-2,mc1-2,sc1-2), ..., c(nn,mn,sn))
    # after ... steps:  list(c(nc1-...,mc1-...,sc1-...), c(nn,mn,sn))
    # after last step:  list(c(nc1-n,mc1-n,sc1-n))
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
  # we have the final vector and we retrieve sc1-n (the combined standard deviation)
  return(res_vec['s'])
}

# clip and mean for one polygon, one raster
# we build a query to get the ST_SummaryStats of the raster values where the polygon
# intersect. After that we summarise to get the stats, using cochrane for calculate the sd
clip_and_mean_simple_case <- function(sf, poly_id, var_name, lidardb) {

  cat(
    crayon::green$bold(glue::glue("Processing {poly_id} polygon..."))
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

  # stats query. In this query, IIUC, we join the raster to the feature on the tiles
  # intersecting, and we calculate the summary stats for the tiles. We return this, as
  # in this way we can calculate not only the mean, but also the std deviation.
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
      !! glue::glue("{var_name}_pixels") := sum(.data[['count']]),
      !! glue::glue("{var_name}_average") := sum(.data[['count']]*.data[['mean']])/sum(.data[['count']]),
      !! glue::glue("{var_name}_min") := min(.data[['min']]),
      !! glue::glue("{var_name}_max") := max(.data[['max']]),
      !! glue::glue("{var_name}_sd") := cochrane_sd_reduce(
        n = .data[['count']], m = .data[['mean']], s = .data[['stddev']]
      ),
      # area covered by raster (km2). Each pixel 20x20m=400m2=4e-04km2
      !! glue::glue("{var_name}_km2") := !! rlang::sym(glue::glue("{var_name}_pixels")) * 4e-04,
      # prop of poly area covered by raster
      !! glue::glue("{var_name}_km2_perc") := 100 * !! rlang::sym(glue::glue("{var_name}_km2")) / (sum(sf::st_area(geometry) %>% as.numeric())/1000000)
    )

  cat(
    crayon::green$bold(glue::glue(" done.")), '\n'
  )

  return(polygon_stats)
}

# clip and mean vectorized for more than one polygon.
# With map_dfr we build a dataframe with the statistics for each polygon supplied
clip_and_mean_vectorized_for_polys <- function(data, id_var_name, var_name, lidardb) {

  # get the geom column name
  sf_column <- attr(data, 'sf_column')
  # rowbinding the summarises
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
  dplyr::slice(supposedly_good_results, 1:2), 'poly_id', 'dbh', lidardb
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
