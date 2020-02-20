## class object creation works ####
test_that("class object creation works", {
  expect_is(lidar(), c('lfcLiDAR'))
  expect_equal(lfcdata:::lfcLiDAR$new(), lidar())
  expect_true(rlang::is_function(lidar()$get_data))
  expect_true(rlang::is_function(lidar()$get_lowres_raster))
  expect_true(rlang::is_function(lidar()$avail_tables))
  expect_true(rlang::is_function(lidar()$describe_var))
  expect_true(rlang::is_function(lidar()$clip_and_stats))
})

# foo to avoid calling the db so often
foo <- lidar()

## get data method works ####
test_that("get_data method works", {
  skip_on_cran()
  skip_on_travis()
  expect_s3_class(foo$get_data('lidar_provincias', c('DBH', 'AB')), 'sf')
  expect_s3_class(foo$get_data('lidar_provincias', 'REC'), 'sf')
  expect_equal(nrow(foo$get_data('lidar_provincias', c('DBH', 'AB'))), 4)
  expect_error(foo$get_data(1, 'REC'), 'not character')
  expect_error(foo$get_data('lidar_provincias', c(1,2)), 'not character')
  expect_error(
    foo$get_data(c('lidar_provincias', 'lidar_municipios'), 'REC'), 'must be of length'
  )
  expect_error(foo$get_data('lidar_provincilities', c('DBH', 'AB')), 'Must be one of')
  expect_error(foo$get_data('lidar_provincias', c('AC')), 'Must be one of')
})

## get_lowres_raster method works ####
test_that("get_lowres_raster method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(foo$get_lowres_raster('AB', 'raster'), 'RasterLayer')
  expect_is(foo$get_lowres_raster(c('AB', 'DBH'), 'raster'), 'RasterBrick')
  expect_is(foo$get_lowres_raster(c('DBH', 'AB'), 'raster'), 'RasterBrick')
  expect_s3_class(foo$get_lowres_raster('AB', 'stars'), 'stars')
  expect_s3_class(foo$get_lowres_raster(c('AB', 'DBH'), 'stars'), 'stars')
  expect_s3_class(foo$get_lowres_raster(c('DBH', 'AB'), 'stars'), 'stars')
  expect_error(foo$get_lowres_raster(1, 'raster'), 'not character')
  expect_error(foo$get_lowres_raster('non_existent_table', 'raster'), 'Must be one of')
  expect_error(foo$get_lowres_raster('AB', 1), 'not character')
})

## avail_tables method works ####
test_that("avail_tables method works", {
  expect_type(foo$avail_tables(), 'character')
  expect_true('lidar_provincias' %in% foo$avail_tables())
})

## describe_var method works ####
test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(foo$describe_var('AB'), c('lfcLiDAR'))
  expect_output(foo$describe_var('AB'))
  expect_output(foo$describe_var(c('AB', 'DBH')))
  expect_error(foo$describe_var(c('AB', 'DBH', 'tururu')), 'Must be one of')
  expect_error(foo$describe_var('tururu'), 'Must be one of')
  expect_error(foo$describe_var(25), 'not character')
})

# sf objects to test
sf_polygons <-
  foo$get_data('lidar_municipios', 'DBH') %>%
  dplyr::slice(1:5) %>%
  dplyr::select(tururu = poly_id)

sf_points <-
  nfi()$get_data('plots', spatial = TRUE) %>%
  dplyr::slice(1:5) %>%
  dplyr::select(plot_id)

## clip_and_stats method works ####
test_that("clip_and_stats method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(foo$clip_and_stats('sf', 'poly_id', c('AB', 'DBH')), 'not a simple feature')
  expect_error(foo$clip_and_stats(sf_polygons, 1, c('AB', 'DBH')), 'not character')
  expect_error(foo$clip_and_stats(sf_polygons, 'poly_id', c(1,2)), 'not character')
  expect_error(
    foo$clip_and_stats(sf_polygons, c('poly_id', 'other_poly_id'), c('AB', 'DBH')),
    'must be of length'
  )
  expect_error(foo$clip_and_stats(sf_polygons, 'poly_id', c('AC', 'DBH')), 'Must be one of')
  expect_true(inherits(foo$clip_and_stats(sf_polygons, 'poly_id', c('AB', 'DBH')), 'sf'))
  expect_identical(
    names(foo$clip_and_stats(sf_polygons, 'poly_id', c('AB', 'DBH'))),
    c(
      'poly_id', 'poly_km2',
      'AB_pixels', 'AB_average', 'AB_min', 'AB_max',
      'AB_sd', 'AB_km2', 'AB_km2_perc',
      'DBH_pixels', 'DBH_average', 'DBH_min', 'DBH_max',
      'DBH_sd', 'DBH_km2', 'DBH_km2_perc',
      'geometry'
    )
  )
  expect_equal(nrow(foo$clip_and_stats(sf_polygons, 'poly_id', c('AB', 'DBH'))), 5)
})

## clip_and_stats method works ####
test_that("point_value method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(foo$point_value('sf', 'plot_id', c('AB', 'DBH')), 'not a simple feature')
  expect_error(foo$point_value(sf_points, 1, c('AB', 'DBH')), 'not character')
  expect_error(foo$point_value(sf_points, 'plot_id', c(1,2)), 'not character')
  expect_error(
    foo$point_value(sf_points, c('plot_id', 'other_plot_id'), c('AB', 'DBH')),
    'must be of length'
  )
  expect_error(foo$point_value(sf_points, 'plot_id', c('AC', 'DBH')), 'Must be one of')
  expect_true(inherits(foo$point_value(sf_points, 'plot_id', c('AB', 'DBH')), 'sf'))
  expect_identical(
    names(foo$point_value(sf_points, 'plot_id', c('AB', 'DBH'))),
    c('plot_id', 'AB', 'DBH', 'geometry')
  )
  expect_equal(nrow(foo$point_value(sf_points, 'plot_id', c('AB', 'DBH'))), 5)
})

## cache works ####
test_that("cache works", {
  skip_on_cran()
  skip_on_travis()
  expect_length(foo$.__enclos_env__$private$data_cache, 3)
  bar <- foo$get_lowres_raster('AB', 'raster')
  expect_is(foo$get_lowres_raster('AB', 'raster'), 'RasterLayer')
  temp_postgresql_conn <- pool::poolCheckout(
    foo$.__enclos_env__$private$pool_conn
  )
  expect_identical(
    bar,
    rpostgis::pgGetRast(
      temp_postgresql_conn, c('public', 'lidar_stack_utm'), bands = 1
    )
  )
  expect_identical(
    foo$get_lowres_raster(c('DBH', 'AB', 'BAT'), 'raster'),
    rpostgis::pgGetRast(
      temp_postgresql_conn, c('public', 'lidar_stack_utm'), bands = c(1,6,2)
    )
  )
  pool::poolReturn(temp_postgresql_conn)
  expect_length(foo$.__enclos_env__$private$data_cache, 4)
  baz <- foo$get_lowres_raster('DBH', 'raster')
  expect_length(foo$.__enclos_env__$private$data_cache, 5)
})

## external methods ####
test_that("external get data wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    foo$get_data(
      'lidar_provincias', c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
    ),
    lidar_get_data(foo, 'lidar_provincias')
  )
  expect_error(
    lidar_get_data('foo', 'lidar_provincias', c('DBH', 'AB')), "class lfcLiDAR"
  )
})

test_that("external get lowres_raster wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    foo$get_lowres_raster('AB', 'raster'), lidar_get_lowres_raster(foo, 'AB', 'raster')
  )
  expect_error(lidar_get_lowres_raster('foo', 'AB', 'raster'), "class lfcLiDAR")
  expect_identical(
    foo$get_lowres_raster(c('REC', 'BAT'), 'stars'),
    lidar_get_lowres_raster(foo, c('REC', 'BAT'), 'stars')
  )
  expect_length(foo$.__enclos_env__$private$data_cache, 6)
})

test_that("external describe_var wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(foo$describe_var('AB'), lidar_describe_var(foo, 'AB'))
  expect_error(lidar_describe_var('foo', 'density'), "class lfcLiDAR")
})

test_that("external clip_and_stats wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    foo$clip_and_stats(sf_polygons, 'poly_id', 'AB'),
    lidar_clip_and_stats(foo, sf_polygons, 'poly_id', 'AB')
  )
  expect_error(lidar_clip_and_stats('foo', sf_polygons, 'poly_id', 'DBH'), "class lfcLiDAR")
})

rm(foo)
