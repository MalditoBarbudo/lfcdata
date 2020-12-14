## class object creation works ####
test_that("class object creation works", {
  expect_is(catdrought(), c('lfcCatDrought'))
  expect_equal(lfcdata:::lfcCatDrought$new(), catdrought())
  expect_true(rlang::is_function(catdrought()$get_data))
  expect_true(rlang::is_function(catdrought()$get_raster))
  expect_true(rlang::is_function(catdrought()$get_current_time_series))
})

#catdroughtdb to avoid call too often
catdroughtdb <- catdrought()
# dates for testing
date_to_check <- as.character(Sys.Date() - sample(1:364, 1))
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

sf_polygons_all_out <- sf_polygons %>%
  dplyr::mutate(
    geometry = geometry + c(500000, 0),
    tururu = paste0("out_", 1:5)
  ) %>%
  sf::st_set_crs(3043)

sf_polygons_one_out <- rbind(sf_polygons, sf_polygons_all_out) %>%
  dplyr::slice(1:6)

sf_polygons_naves <- lidar()$get_data('lidar_municipalities', 'DBH') %>%
  dplyr::filter(poly_id == 'Navès')

## get data method works ####
test_that("get_data method works", {
  # get method is not implemented in catdrought db, so it must print a message
  # and return self
  expect_output(catdroughtdb$get_data(), 'No get_data method')
  expect_equal(catdroughtdb$get_data(), catdroughtdb)
})

## get_raster method works ####
test_that("get_raster method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(
    catdroughtdb$get_raster(date_to_check, 'raster'), 'RasterBrick'
  )
  expect_s3_class(
    catdroughtdb$get_raster(date_to_check, 'stars'), 'stars'
  )
  expect_error(catdroughtdb$get_raster(25, 'stars'), "not character")
  expect_error(catdroughtdb$get_raster(date_to_check, 25), "not character")
  expect_error(
    catdroughtdb$get_raster(date_to_check, c('stars', 'raster')),
    'must be of length'
  )
  expect_error(
    catdroughtdb$get_raster(c(date_to_check, date_to_check), 'stars'),
    'must be of length'
  )
  expect_error(
    catdroughtdb$get_raster(date_to_check, 'tururu'),
    "Must be one of"
  )
  expect_error(
    catdroughtdb$get_raster(as.character(Sys.Date()), 'stars'),
    "Selected date"
  )
  expect_true(
    all(
      names(catdroughtdb$get_raster(date_to_check, 'stars')) %in%
        c(
          'DDS', 'DeepDrainage', 'Eplant', 'Esoil', 'Infiltration',
          'Interception', 'LAI', 'LMFC', 'PET', 'Precipitation', 'Psi', 'REW',
          'Runoff', 'Theta'
        )
    )
  )
})

## get_current_time_series method works ####
test_that("get_current_time_series method works", {

  ## general errors
  expect_error(
    catdroughtdb$get_current_time_series('sf', 'Esoil'),
    'not a simple feature'
  )
  expect_error(
    catdroughtdb$get_current_time_series(sf_points, c('Esoil', 'Theta')),
    'must be of length'
  )
  expect_error(
    catdroughtdb$get_current_time_series('sf', 25),
    'not character'
  )
  expect_error(
    catdroughtdb$get_current_time_series(sf_polygons, 'tururu'),
    "Must be one of"
  )

  # ok
  expect_is(
    catdroughtdb$get_current_time_series(sf_points, 'Esoil'),
    'data.frame'
  )
  expect_is(
    catdroughtdb$get_current_time_series(sf_points_3043, 'Esoil'),
    'data.frame'
  )
  expect_is(
    catdroughtdb$get_current_time_series(sf_polygons, 'Esoil'),
    'data.frame'
  )
  expect_true(
    all(names(catdroughtdb$get_current_time_series(sf_polygons, 'Esoil')) %in%
      c('day', 'polygon_id', 'count', 'mean', 'stddev', 'min', 'max', 'stderror'))
  )
  # work with multipolygon (two rows for each quantile)
  expect_is(
    catdroughtdb$get_current_time_series(sf_polygons_naves, 'Esoil'),
    'data.frame'
  )

  # one out
  expect_warning(
    catdroughtdb$get_current_time_series(sf_points_one_out, 'Esoil'),
    'One or more'
  )
  expect_warning(
    catdroughtdb$get_current_time_series(sf_polygons_one_out, 'Esoil'),
    'One or more'
  )

  # all out
  expect_error(
    catdroughtdb$get_current_time_series(sf_points_all_out, 'Esoil'),
    'All points'
  )
  expect_error(
    catdroughtdb$get_current_time_series(sf_polygons_all_out, 'Esoil'),
    'All polygons'
  )

})
