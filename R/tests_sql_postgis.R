library(lfcdata)
lidardb <- lidar()

supposedly_good_results <- sf::read_sf(
  lidardb$.__enclos_env__$private$pool_conn, 'lidar_municipios'
) %>%
  dplyr::select(poly_id, mean_dbh)

wkt_polys <-
  sf::read_sf(lidardb$.__enclos_env__$private$pool_conn, 'lidar_municipios') %>%
  dplyr::select(poly_id, geometry) %>%
  dplyr::slice(1:5) %>%
  sf::st_geometry() %>%
  sf::st_as_text(EWKT = TRUE)

names_polys <- c(
  "Abella de la Conca", "Abrera", "Àger", "Agramunt", "Aguilar de Segarra"
)

vars <- c("public.dbh")

feat_query <- glue::glue_sql(
  "SELECT {names_polys[1]} As poly_id, ST_GeomFromEWKT({wkt_polys[1]}) As geometry",
  .con = lidardb$.__enclos_env__$private$pool_conn
)

sf::st_read(
  lidardb$.__enclos_env__$private$pool_conn, query = feat_query, as_tibble = TRUE
)

b_stats_query <- glue::glue_sql(
  "SELECT poly_id, geometry, (ST_SummaryStats(ST_Clip(rast,1,geometry, true))).*
         FROM dbh
       INNER JOIN ({feat_query}) AS feat
       ON ST_Intersects(feat.geometry,rast)",
  .con = lidardb$.__enclos_env__$private$pool_conn
)

sf::st_read(
  lidardb$.__enclos_env__$private$pool_conn, query = b_stats_query, as_tibble = TRUE
)

# b_stats_query <- glue::glue_sql(
#   "SELECT poly_id, geometry, (stats).* FROM ({foo_query}) AS foo",
#   .con = lidardb$.__enclos_env__$private$pool_conn
# )
#
# sf::st_read(
#   lidardb$.__enclos_env__$private$pool_conn, query = b_stats_query, as_tibble = TRUE
# ) %>%
#   dplyr::group_by(poly_id) %>% dplyr::mutate(sum(sum)/sum(count))

lidar_query <- glue::glue_sql(
  "SELECT poly_id, geometry, SUM(sum)/SUM(count) As mean_dbh
    FROM ({b_stats_query}) AS b_stats
   WHERE count > 0
   GROUP BY poly_id, geometry
   ORDER BY poly_id;",
  .con = lidardb$.__enclos_env__$private$pool_conn
)

sf::st_read(
  lidardb$.__enclos_env__$private$pool_conn, query = lidar_query, as_tibble = TRUE
)

# lidar_query <- glue::glue(
#   "WITH
#      feat AS (SELECT '{names_polys}' As poly_id, ST_GeomFromEWKT('{wkt_polys}') As geometry),
#      b_stats AS (SELECT poly_id, geometry, (stats).* FROM (
#        SELECT poly_id, geometry, ST_SummaryStats(ST_Clip(rast,1,geometry, true)) As stats
#          FROM {tolower(vars)}
#        INNER JOIN feat
#        ON ST_Intersects(feat.geometry,rast)
#      ) As foo)
#    SELECT poly_id, geometry, SUM(mean*count)/SUM(count) As mean_{vars} FROM b_stats
#      WHERE count > 0
#    GROUP BY poly_id, geometry
#    ORDER BY poly_id;",
#   .con = lidardb$.__enclos_env__$private$pool_conn
# )

# get and join the results
res <- lidar_query %>%
  purrr::map(
    ~ sf::st_read(lidardb$.__enclos_env__$private$pool_conn, query = .x) %>% dplyr::as_tibble()
  ) %>%
  purrr::reduce(dplyr::left_join, by = c('poly_id')) %>%
  dplyr::select(poly_id, dplyr::starts_with('mean_'), geometry = geometry.x) %>%
  sf::st_as_sf()

# lidardb$.__enclos_env__$private$pool_conn


lidardb$get_data('DBH', 'stars') %>%
  sf::st_crop(supposedly_good_results[['geometry']][1]) %>%
  plot(reset = FALSE)
plot(supposedly_good_results[['geometry']][1], add = TRUE, col = NA, border = 'green')

foo <- stars::read_stars(
  '../03_lidar_app/lidarappkg/data-raw/DBH.tif', proxy = TRUE
)

sf::st_crop(foo, supposedly_good_results[['geometry']][1]) %>%
  stars::st_as_stars() %>%
  sf::st_crop(supposedly_good_results[['geometry']][1]) %>%
  purrr::map(~ structure(.x, dim = NULL)) %>%
  tibble::as_tibble() %>%
  dplyr::summarise_all(.funs = mean, na.rm = TRUE)


  plot(reset = FALSE)
plot(supposedly_good_results[['geometry']][1], add = TRUE, col = NA, border = 'green')

  stars::
  purrr::map(~ structure(.x, dim = NULL)) %>%
  tibble::as_tibble() %>%
  dplyr::summarise_all(.funs = mean, na.rm = TRUE)

seq_along(supposedly_good_results[['geometry']][1:5]) %>%
  purrr::map_dfr(
    .f = ~ sf::st_crop(foo, supposedly_good_results[['geometry']][.x]) %>%
      purrr::map(~ structure(.x, dim = NULL)) %>%
      tibble::as_tibble() %>%
      dplyr::summarise_all(.funs = mean, na.rm = TRUE)
  )

sf::st_read(
  lidardb$.__enclos_env__$private$pool_conn,
  as_tibble = TRUE,
  query = glue::glue_sql(
    "SELECT ST_Clip(rast,1,ST_GeomFromEWKT({wkt_polys[1]}), true) FROM dbh
       INNER JOIN ({feat_query}) AS feat
       ON ST_Intersects(feat.geometry,rast);",
    .con = lidardb$.__enclos_env__$private$pool_conn
  )
)
