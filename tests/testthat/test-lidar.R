## class object creation works ####
test_that("class object creation works", {
  expect_is(lidar(), c('lfcLiDAR'))
  # expect_equal(lfcdata:::lfcLiDAR$new(), lidar())
  expect_true(rlang::is_function(lidar()$get_data))
  expect_true(rlang::is_function(lidar()$get_lowres_raster))
  expect_true(rlang::is_function(lidar()$avail_tables))
  expect_true(rlang::is_function(lidar()$describe_var))
  expect_true(rlang::is_function(lidar()$clip_and_stats))
})

# lidardb to avoid calling the db so often
lidardb <- lidar()

## get data method works ####
test_that("get_data method works", {
  skip_on_cran()
  skip_on_travis()
  expect_s3_class(lidardb$get_data('lidar_provinces', c('DBH', 'AB')), 'sf')
  expect_s3_class(lidardb$get_data('lidar_provinces', 'REC'), 'sf')
  expect_equal(nrow(lidardb$get_data('lidar_provinces', c('DBH', 'AB'))), 4)
  expect_error(lidardb$get_data(1, 'REC'), 'not character')
  expect_error(lidardb$get_data('lidar_provinces', c(1,2)), 'not character')
  expect_error(
    lidardb$get_data(c('lidar_provinces', 'lidar_municipalities'), 'REC'), 'must be of length'
  )
  expect_error(lidardb$get_data('lidar_provincilities', c('DBH', 'AB')), 'Must be one of')
  expect_error(lidardb$get_data('lidar_provinces', c('AC')), 'Must be one of')
  expect_equal(ncol(lidardb$get_data('lidar_provinces', 'REC')), 10)
  expect_true(all(
    c('DBH_pixels', 'AB_pixels', 'DBH_sd', 'AB_sd') %in%
      names(lidardb$get_data('lidar_provinces', c('DBH', 'AB')))
  ))
  expect_true(all(
    c(
      'DBH_pixels', 'AB_pixels', 'REC_pixels', 'VAE_pixels', 'BAT_pixels',
      'BF_pixels', 'CAT_pixels', 'HM_pixels', 'DEN_pixels', 'LAI_pixels'
    ) %in% names(lidardb$get_data('lidar_provinces', 'all'))
  ))
  expect_true(all(
    c(
      'DBH_pixels', 'AB_pixels', 'REC_pixels', 'VAE_pixels', 'BAT_pixels',
      'BF_pixels', 'CAT_pixels', 'HM_pixels', 'DEN_pixels', 'LAI_pixels'
    ) %in% names(lidardb$get_data('lidar_provinces', c('all', 'DBH')))
  ))
})

## get_lowres_raster method works ####
test_that("get_lowres_raster method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(lidardb$get_lowres_raster('AB', 'raster'), 'RasterLayer')
  expect_is(lidardb$get_lowres_raster(c('AB', 'DBH'), 'raster'), 'RasterBrick')
  expect_is(lidardb$get_lowres_raster(c('DBH', 'AB'), 'raster'), 'RasterBrick')
  expect_s3_class(lidardb$get_lowres_raster('AB', 'stars'), 'stars')
  expect_s3_class(lidardb$get_lowres_raster(c('AB', 'DBH'), 'stars'), 'stars')
  expect_s3_class(lidardb$get_lowres_raster(c('DBH', 'AB'), 'stars'), 'stars')
  expect_error(lidardb$get_lowres_raster(1, 'raster'), 'not character')
  expect_error(lidardb$get_lowres_raster('non_existent_table', 'raster'), 'Must be one of')
  expect_error(lidardb$get_lowres_raster('AB', 1), 'not character')
})

## avail_tables method works ####
test_that("avail_tables method works", {
  expect_type(lidardb$avail_tables(), 'character')
  expect_true('lidar_provinces' %in% lidardb$avail_tables())
  expect_true('lidar_pein' %in% lidardb$avail_tables())
})

## describe_var method works ####
test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(lidardb$describe_var('AB'), c('lfcLiDAR'))
  expect_output(lidardb$describe_var('AB'))
  expect_output(lidardb$describe_var(c('AB', 'DBH')))
  expect_error(lidardb$describe_var(c('AB', 'DBH', 'tururu')), 'Must be one of')
  expect_error(lidardb$describe_var('tururu'), 'Must be one of')
  expect_error(lidardb$describe_var(25), 'not character')
})

# sf objects to test
sf_polygons <-
  lidardb$get_data('lidar_municipalities', 'DBH') %>%
  dplyr::slice(1:5) %>%
  dplyr::select(tururu = poly_id, geom = geometry)

sf_points <-
  nfi()$get_data('plots', spatial = TRUE) %>%
  dplyr::slice(1:5) %>%
  dplyr::select(plot_id)

sf_multipoints <-
  dplyr::tibble(
    point_id = 'wrong',
    geometry = sf::st_multipoint(matrix(1:10, , 2)) %>% sf::st_sfc()
  ) %>%
  sf::st_as_sf(sf_column_name = 'geometry')

sf_polygons_latlong <-
  sf_polygons %>% sf::st_transform(crs = 4326)

sf_empty_polygon <-
  lidardb$get_data('lidar_xn2000', 'DBH') %>%
  dplyr::slice(19) %>%
  dplyr::select(poly_id)

## clip_and_stats method works ####
test_that("clip_and_stats method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(lidardb$clip_and_stats('sf', 'tururu', c('AB', 'DBH')), 'not a simple feature')
  expect_error(lidardb$clip_and_stats(sf_polygons, 1, c('AB', 'DBH')), 'not character')
  expect_error(lidardb$clip_and_stats(sf_polygons, 'tururu', c(1,2)), 'not character')
  expect_error(
    lidardb$clip_and_stats(sf_polygons, c('tururu', 'other_tururu'), c('AB', 'DBH')),
    'must be of length'
  )
  expect_error(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AC', 'DBH')), 'Must be one of')
  expect_error(lidardb$clip_and_stats(sf_polygons, 'fake_id', 'AB'), 'Must be one of')
  expect_error(
    lidardb$clip_and_stats(sf_multipoints, 'point_id', c('AB', 'DBH')),
    'not a POLYGON or a MULTIPOLYGON'
  )
  expect_true(inherits(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AB', 'DBH')), 'sf'))
  expect_identical(
    names(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AB', 'DBH'))),
    c(
      'tururu', 'poly_km2',
      'AB_pixels', 'AB_average', 'AB_sd', 'AB_min', 'AB_max',
      'AB_km2', 'AB_km2_perc',
      'DBH_pixels', 'DBH_average', 'DBH_sd', 'DBH_min', 'DBH_max',
      'DBH_km2', 'DBH_km2_perc',
      'geom'
    )
  )
  expect_equal(nrow(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AB', 'DBH'))), 5)
  expect_equal(
    sf::st_crs(lidardb$clip_and_stats(sf_polygons_latlong, 'tururu', c('AB', 'DBH'))),
    sf::st_crs(4326)
  )
  expect_equal(
    sf::st_crs(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AB', 'DBH'))),
    sf::st_crs(3043)
  )
  expect_equal(
    lidardb$clip_and_stats(sf_empty_polygon, 'poly_id', c('AB', 'DBH'))$poly_km2,
    as.numeric(sf::st_area(sf_empty_polygon)) / 1000000
  )
  expect_true(
    is.na(lidardb$clip_and_stats(sf_empty_polygon, 'poly_id', c('AB', 'DBH'))$AB_average)
  )
  expect_equal(
    nrow(lidardb$clip_and_stats(sf_empty_polygon, 'poly_id', c('AB', 'REC'))),
    1
  )
})

## point_value method works ####
test_that("point_value method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(lidardb$point_value('sf', 'plot_id', c('AB', 'DBH')), 'not a simple feature')
  expect_error(lidardb$point_value(sf_points, 1, c('AB', 'DBH')), 'not character')
  expect_error(lidardb$point_value(sf_points, 'plot_id', c(1,2)), 'not character')
  expect_error(
    lidardb$point_value(sf_points, c('plot_id', 'other_plot_id'), c('AB', 'DBH')),
    'must be of length'
  )
  expect_error(lidardb$point_value(sf_points, 'plot_id', c('AC', 'DBH')), 'Must be one of')
  expect_error(lidardb$point_value(sf_points, 'fake_id', 'AB'), 'Must be one of')
  expect_error(
    lidardb$point_value(sf_multipoints, 'point_id', c('AB', 'DBH')), 'not a POINT'
  )
  expect_true(inherits(lidardb$point_value(sf_points, 'plot_id', c('AB', 'DBH')), 'sf'))
  expect_identical(
    names(lidardb$point_value(sf_points, 'plot_id', c('AB', 'DBH'))),
    c('plot_id', 'AB', 'DBH', 'geometry')
  )
  expect_equal(nrow(lidardb$point_value(sf_points, 'plot_id', c('AB', 'DBH'))), 5)
})

## cache works ####
test_that("cache works", {
  skip_on_cran()
  skip_on_travis()
  expect_length(lidardb$.__enclos_env__$private$data_cache, 5)
  bar <- lidardb$get_lowres_raster('AB', 'raster')
  expect_is(lidardb$get_lowres_raster('AB', 'raster'), 'RasterLayer')
  temp_postgresql_conn <- pool::poolCheckout(
    lidardb$.__enclos_env__$private$pool_conn
  )
  expect_identical(
    bar,
    rpostgis::pgGetRast(
      temp_postgresql_conn, c('public', 'lidar_stack_utm'), bands = 1
    )
  )
  expect_identical(
    lidardb$get_lowres_raster(c('DBH', 'AB', 'BAT'), 'raster'),
    rpostgis::pgGetRast(
      temp_postgresql_conn, c('public', 'lidar_stack_utm'), bands = c(1,8,2)
    )
  )
  pool::poolReturn(temp_postgresql_conn)
  expect_length(lidardb$.__enclos_env__$private$data_cache, 6)
  baz <- lidardb$get_lowres_raster('DBH', 'raster')
  expect_length(lidardb$.__enclos_env__$private$data_cache, 7)
})

## external methods ####
test_that("external get data wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    lidardb$get_data(
      'lidar_provinces', c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'DEN', 'HM', 'LAI', 'REC', 'VAE')
    ),
    lidar_get_data(lidardb, 'lidar_provinces')
  )
  expect_error(
    lidar_get_data('lidardb', 'lidar_provinces', c('DBH', 'AB')), "class lfcLiDAR"
  )
})

test_that("external get lowres_raster wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    lidardb$get_lowres_raster('AB', 'raster'), lidar_get_lowres_raster(lidardb, 'AB', 'raster')
  )
  expect_error(lidar_get_lowres_raster('lidardb', 'AB', 'raster'), "class lfcLiDAR")
  expect_identical(
    lidardb$get_lowres_raster(c('REC', 'BAT'), 'stars'),
    lidar_get_lowres_raster(lidardb, c('REC', 'BAT'), 'stars')
  )
  expect_length(lidardb$.__enclos_env__$private$data_cache, 8)
})

test_that("external describe_var wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(lidardb$describe_var('AB'), lidar_describe_var(lidardb, 'AB'))
  expect_error(lidar_describe_var('lidardb', 'density'), "class lfcLiDAR")
})

test_that("external clip_and_stats wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    lidardb$clip_and_stats(sf_polygons, 'tururu', 'AB'),
    lidar_clip_and_stats(lidardb, sf_polygons, 'tururu', 'AB')
  )
  expect_error(lidar_clip_and_stats('lidardb', sf_polygons, 'tururu', 'DBH'), "class lfcLiDAR")
})

test_that("external point_value wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    lidardb$point_value(sf_points, 'plot_id', 'AB'),
    lidar_point_value(lidardb, sf_points, 'plot_id', 'AB')
  )
  expect_error(lidar_point_value('lidardb', sf_points, 'plot_id', 'DBH'), "class lfcLiDAR")
})

rm(lidardb)
