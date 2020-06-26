## class object creation works ####
test_that("class object creation works", {
  expect_is(meteoland(), c('lfcMeteoland'))
  expect_equal(lfcdata:::lfcMeteoland$new(), meteoland())
  expect_true(rlang::is_function(meteoland()$get_data))
  expect_true(rlang::is_function(meteoland()$points_interpolation))
  expect_true(rlang::is_function(meteoland()$raster_interpolation))
})

# lidardb to avoid calling the db so often
meteolanddb <- meteoland()
# dates for current are 365 natural days, so if we fix a date for tests it will
# be rong at some point. Is better to relay on Sys.Date for this tests.
# start_date <- as.character(Sys.Date()-10)
# end_date <- as.character(Sys.Date()-8)
# TODO update this when all raster tables are availbale

start_date <- '2020-04-25'
end_date <- '2020-04-27'

## get data method works ####
test_that("get_data method works", {
  # get method is not implemented in meteoland db, so it must print a message
  # and return self
  expect_output(meteolanddb$get_data(), 'No get_data method')
  expect_equal(meteolanddb$get_data(), meteolanddb)
})

## get lowres raster method works ####
test_that("get_lowres_raster method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(meteolanddb$get_lowres_raster(start_date, 'raster'), 'RasterBrick')
  expect_s3_class(meteolanddb$get_lowres_raster(start_date, 'stars'), 'stars')
  expect_error(meteolanddb$get_lowres_raster(25, 'stars'), "not character")
  expect_error(meteolanddb$get_lowres_raster(start_date, 25), "not character")
  expect_error(
    meteolanddb$get_lowres_raster(start_date, c('stars', 'raster')),
    'must be of length'
  )
  expect_error(
    meteolanddb$get_lowres_raster(c(start_date, end_date), 'stars'),
    'must be of length'
  )
  expect_error(
    meteolanddb$get_lowres_raster(start_date, 'tururu'),
    "Must be one of"
  )
})

# sf objects to test
sf_polygons <-
  lidar()$get_data('lidar_municipalities', 'DBH') %>%
  dplyr::slice(1:5) %>%
  dplyr::select(tururu = poly_id)

sf_points <-
  nfi()$get_data('plots', spatial = TRUE) %>%
  dplyr::slice(1:5) %>%
  dplyr::select(plot_id)

sf_points_3043 <- sf::st_transform(sf_points, crs = 3043)

sf_points_all_out <- sf_points %>%
  dplyr::mutate(geometry = geometry + 10, plot_id = paste0('out', 1:5)) %>%
  sf::st_set_crs(4326)

sf_points_one_out <- rbind(sf_points, sf_points_all_out %>% dplyr::slice(1))

sf_multipoints <-
  dplyr::tibble(
    point_id = 'wrong',
    geometry = sf::st_multipoint(matrix(1:10, , 2)) %>% sf::st_sfc()
  ) %>%
  sf::st_as_sf(sf_column_name = 'geometry')

sf_polygons_latlong <-
  sf_polygons %>% sf::st_transform(crs = 4326)

sf_empty_polygon <-
  lidar()$get_data('lidar_xn2000', 'DBH') %>%
  dplyr::slice(19) %>%
  dplyr::select(poly_id)

## points interpolation works ####
test_that("points_interpolation method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(
    meteolanddb$points_interpolation('sf', c(start_date, end_date)),
    'not a simple feature'
  )
  expect_error(
    meteolanddb$points_interpolation(sf_points, c(start_date)),
    'must be of length'
  )
  expect_error(
    meteolanddb$points_interpolation(sf_points, c(25, 26)),
    'not character'
  )
  expect_error(
    meteolanddb$points_interpolation(sf_points, c('tururu', 'larara')),
    'cannot be converted to date'
  )
  expect_error(
    meteolanddb$points_interpolation(sf_polygons, c(start_date, end_date)),
    'is not a POINT'
  )
  expect_is(
    meteolanddb$points_interpolation(sf_points, c(start_date, end_date)),
    'SpatialPointsMeteorology'
  )
  # we need an ok interpolation for testing throughfully
  ok_interpolation <-
    meteolanddb$points_interpolation(sf_points, c(start_date, end_date))

  expect_length(ok_interpolation@dates, 3)
  expect_length(ok_interpolation@data, 5)
  expect_equal(nrow(ok_interpolation@data[[1]]), 3)
  expect_equal(ncol(ok_interpolation@data[[1]]), 12)


  expect_warning(
    meteolanddb$points_interpolation(
      sf_points, c(as.character(Sys.Date()-2), as.character(Sys.Date()+1))
    ), "Some dates"
  )
  one_day_missing_interpolation <-
    meteolanddb$points_interpolation(
      sf_points, c(as.character(Sys.Date()-2), as.character(Sys.Date()))
    )

  expect_length(one_day_missing_interpolation@dates, 2)
  expect_length(one_day_missing_interpolation@data, 5)
  expect_equal(nrow(one_day_missing_interpolation@data[[1]]), 2)
  expect_equal(ncol(one_day_missing_interpolation@data[[1]]), 12)

  # when all dates are out of range, then error occurs
  expect_error(
    meteolanddb$points_interpolation(
      sf_points, c(as.character(Sys.Date()-369), as.character(Sys.Date()-367))
    ), "No meteo data found"
  )

  expect_warning(
    meteolanddb$points_interpolation(sf_points_one_out, c(start_date, end_date)),
    "Some points"
  )
  one_coord_missing_interpolation <-
    meteolanddb$points_interpolation(sf_points_one_out, c(start_date, end_date))

  expect_length(one_coord_missing_interpolation@dates, 3)
  expect_length(one_coord_missing_interpolation@data, 5)
  expect_equal(nrow(one_coord_missing_interpolation@data[[1]]), 3)
  expect_equal(ncol(one_coord_missing_interpolation@data[[1]]), 12)

  expect_error(
    meteolanddb$points_interpolation(sf_points_all_out, c(start_date, end_date)),
    "All coordinates are not in Catalonia"
  )

  expect_identical(
    meteolanddb$points_interpolation(sf_points, c(start_date, end_date)),
    meteolanddb$points_interpolation(sf_points_3043, c(start_date, end_date))
  )
})

## raster interolation works
test_that("raster_interpolation method works", {
  skip_on_cran()
  skip_on_travis()



})
