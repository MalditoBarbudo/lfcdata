wkt_polys <-
  sf::read_sf('~/Descargas/user_polygons.gpkg') %>%
  sf::st_transform(crs = 3043) %>%
  sf::st_geometry() %>%
  sf::st_as_text(EWKT = TRUE)

names_polys <- c("Portbou", "Lles de Cerdanya", "Sant Feliu de Codines", "Vilaplana", "Puigpelat")

vars <- c("public.dbh")

feat_query <- glue::glue_sql(
  "SELECT {names_polys[1]} As poly_id, ST_GeomFromEWKT({wkt_polys[1]}) As geometry",
  .con = lidardb$.__enclos_env__$private$pool_conn
)

foo_query <- glue::glue_sql(
  "SELECT poly_id, geometry, ST_SummaryStats(ST_Clip(rast,1,geometry, true)) As stats
         FROM dbh
       INNER JOIN ({feat_query}) AS feat
       ON ST_Intersects(feat.geometry,rast)",
  .con = lidardb$.__enclos_env__$private$pool_conn
)

b_stats_query <- glue::glue_sql(
  "SELECT poly_id, geometry, (stats).* FROM ({foo_query}) AS foo",
  .con = lidardb$.__enclos_env__$private$pool_conn
)

lidar_query <- glue::glue_sql(
  "SELECT poly_id, geometry, SUM(mean*count)/SUM(count) As mean_dbh
    FROM ({b_stats_query}) AS b_stats
     WHERE count > 0
   GROUP BY poly_id, geometry
   ORDER BY poly_id;",
  .con = lidardb$.__enclos_env__$private$pool_conn
)

lidar_query <- glue::glue(
  "WITH
     feat AS (SELECT '{names_polys}' As poly_id, ST_GeomFromEWKT('{wkt_polys}') As geometry),
     b_stats AS (SELECT poly_id, geometry, (stats).* FROM (
       SELECT poly_id, geometry, ST_SummaryStats(ST_Clip(rast,1,geometry, true)) As stats
         FROM {tolower(vars)}
       INNER JOIN feat
       ON ST_Intersects(feat.geometry,rast)
     ) As foo)
   SELECT poly_id, geometry, SUM(mean*count)/SUM(count) As mean_{vars} FROM b_stats
     WHERE count > 0
   GROUP BY poly_id, geometry
   ORDER BY poly_id;",
  .con = lidardb$.__enclos_env__$private$pool_conn
)

# get and join the results
res <- lidar_query %>%
  purrr::map(
    ~ sf::st_read(lidardb$.__enclos_env__$private$pool_conn, query = .x) %>% dplyr::as_tibble()
  ) %>%
  purrr::reduce(dplyr::left_join, by = c('poly_id')) %>%
  dplyr::select(poly_id, dplyr::starts_with('mean_'), geometry = geometry.x) %>%
  sf::st_as_sf()

# lidardb$.__enclos_env__$private$pool_conn
