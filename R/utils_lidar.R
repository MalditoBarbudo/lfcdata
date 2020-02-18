#
#
# # clip and mean for one polygon, one raster
# # we build a query to get the ST_SummaryStats of the raster values where the polygon
# # intersect. After that we summarise to get the stats, using cochrane for calculate the sd
# clip_and_stats_simple_case <- function(sf, poly_id, var_name, lidardb) {
#
#   cat(
#     crayon::green$bold(glue::glue("Processing {poly_id} polygon for {var_name}..."))
#   )
#
#   # poly as wkt, to avoid table creation
#   wkt_poly <-
#     sf %>%
#     sf::st_geometry() %>%
#     sf::st_as_text(EWKT = TRUE)
#
#   # feature query. In this query we create the simple feature table-like
#   feat_query <- glue::glue_sql(
#     "SELECT {poly_id} As poly_id, ST_GeomFromEWKT({wkt_poly}) As geometry",
#     .con = lidardb$.__enclos_env__$private$pool_conn
#   )
#
#   # stats query. In this query, IIUC, we join the raster to the feature on the tiles
#   # intersecting, and we calculate the summary stats for the tiles. We return this, as
#   # in this way we can calculate not only the mean, but also the std deviation.
#   b_stats_query <- glue::glue_sql(
#     "SELECT poly_id, geometry, (ST_SummaryStats(ST_Clip(rast,1,geometry, true),1,true)).*
#          FROM {`var_name`}
#        INNER JOIN ({feat_query}) AS feat
#        ON ST_Intersects(feat.geometry,rast)",
#     .con = lidardb$.__enclos_env__$private$pool_conn
#   )
#
#   # execute the query and retrieve the data
#   intersecting_tiles_stats <- sf::st_read(
#     lidardb$.__enclos_env__$private$pool_conn, query = b_stats_query, as_tibble = TRUE
#   )
#
#   polygon_stats <-
#     intersecting_tiles_stats %>%
#     dplyr::as_tibble() %>%
#     dplyr::mutate(count = as.integer(count)) %>%
#     dplyr::filter(count > 0) %>%
#     dplyr::group_by(poly_id) %>%
#     dplyr::summarise(
#       # area of the polygon
#       poly_km2 = (sf::st_area(dplyr::first(.data[['geometry']])) %>% as.numeric()) / 1000000,
#       # regular stats
#       !! glue::glue("{var_name}_pixels") := sum(.data[['count']]),
#       !! glue::glue("{var_name}_average") := sum(.data[['count']]*.data[['mean']])/sum(.data[['count']]),
#       !! glue::glue("{var_name}_min") := min(.data[['min']]),
#       !! glue::glue("{var_name}_max") := max(.data[['max']]),
#       !! glue::glue("{var_name}_sd") := cochrane_sd_reduce(
#         n = .data[['count']], m = .data[['mean']], s = .data[['stddev']]
#       ),
#       # area covered by raster (km2). Each pixel 20x20m=400m2=4e-04km2
#       !! glue::glue("{var_name}_km2") := !! rlang::sym(glue::glue("{var_name}_pixels")) * 4e-04,
#       # prop of poly area covered by raster
#       !! glue::glue("{var_name}_km2_perc") := 100 * !! rlang::sym(glue::glue("{var_name}_km2")) / poly_km2
#     )
#
#   cat(
#     crayon::green$bold(glue::glue(" done.")), '\n'
#   )
#
#   return(polygon_stats)
# }
#
# # clip and mean vectorized for more than one polygon.
# # With map_dfr we build a dataframe with the statistics for each polygon supplied
# clip_and_stats_vectorized_for_polys <- function(data, id_var_name, var_name, lidardb) {
#
#   # get the geom column name
#   sf_column <- attr(data, 'sf_column')
#   # rowbinding the summarises
#   summ_polys_data <-
#     seq_along(data[[sf_column]]) %>%
#     purrr::map_dfr(
#       ~ clip_and_stats_simple_case(
#         sf = data[[sf_column]][.x],
#         poly_id = data[[id_var_name]][.x],
#         var_name = var_name,
#         lidardb = lidardb
#       )
#     )
#
#   return(summ_polys_data)
# }
#
# # clip and stats
# clip_and_stats <- function(data, id_var_name, vars_name, lidardb) {
#   vars_name %>%
#     purrr::map(~ clip_and_stats_vectorized_for_polys(data, id_var_name, .x, lidardb)) %>%
#     purrr::reduce(
#       .f = dplyr::full_join,
#       by = c('poly_id', 'poly_km2')
#     )
# }


# supposedly_good_results <- sf::read_sf(
#   lidardb$.__enclos_env__$private$pool_conn, 'lidar_municipios'
# ) %>%
#   dplyr::select(poly_id, mean_dbh)
#
# library(tictoc)
# tic()
# clip_and_stats_vectorized_for_polys(
#   dplyr::slice(supposedly_good_results, 1:50), 'poly_id', 'dbh', lidardb
# )
# toc()
# sum(dplyr::slice(supposedly_good_results, 1:50) %>% sf::st_area())/1000000
#
# tic()
# clip_and_stats_vectorized_for_polys(
#   supposedly_good_results, 'poly_id', 'dbh', lidardb
# )
# toc()
# sum(supposedly_good_results %>% sf::st_area())/1000000
#
# provincias_test <- sf::read_sf(
#   lidardb$.__enclos_env__$private$pool_conn, 'lidar_provincias'
# ) %>%
#   dplyr::select(poly_id, mean_dbh)
# tic()
# clip_and_stats_vectorized_for_polys(
#   provincias_test, 'poly_id', 'dbh', lidardb
# )
# toc()
#
# sum(provincias_test %>% sf::st_area())/1000000
#
# tic()
# clip_and_stats(dplyr::slice(provincias_test, 1:4), 'poly_id', c('dbh', 'ab'), lidardb)
# toc()
