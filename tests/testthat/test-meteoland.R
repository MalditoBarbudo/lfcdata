## class object creation works ####
test_that("class object creation works", {
  expect_true(inherits(meteoland(), c('lfcMeteoland')))
  # expect_equal(lfcdata:::lfcMeteoland$new(), meteoland())
  expect_true(rlang::is_function(meteoland()$get_data))
  expect_true(rlang::is_function(meteoland()$points_interpolation))
  expect_true(rlang::is_function(meteoland()$raster_interpolation))
})

# meteolanddb to avoid calling the db so often
meteolanddb <- meteoland()
# dates for current are 365 natural days, so if we fix a date for tests it will
# be wrong at some point. Is better to relay on Sys.Date for this tests.
start_date <- as.character(Sys.Date() - 10)
end_date <- as.character(Sys.Date() - 8)
# dates for historical methods
historical_start_date <- '1981-04-25'
historical_end_date <- '1981-04-27'

# sf objects to test
sf_polygons <-
  lidar()$get_data('lidar_municipalities', 'DBH') |>
  dplyr::slice(1:5) |>
  dplyr::select(tururu = poly_id)

sf_points_elevation <-
  nfi()$get_data('plots', spatial = TRUE) |>
  dplyr::slice(1:5) |>
  dplyr::select(plot_id, elevation = topo_altitude_asl)

sf_points <- sf_points_elevation |>
  dplyr::select(plot_id)

sf_points_3043 <- sf::st_transform(sf_points, crs = 3043)

sf_points_all_out <- sf_points |>
  dplyr::mutate(geometry = geometry + 10, plot_id = paste0('out', 1:5)) |>
  sf::st_set_crs(4326)

sf_points_one_out <- rbind(sf_points, sf_points_all_out |> dplyr::slice(1))

sf_multipoints <-
  dplyr::tibble(
    point_id = 'wrong',
    geometry = sf::st_multipoint(matrix(1:10, , 2)) |> sf::st_sfc()
  ) |>
  sf::st_as_sf(sf_column_name = 'geometry')

sf_polygons_latlong <-
  sf_polygons |> sf::st_transform(crs = 4326)

sf_empty_polygon <-
  lidar()$get_data('lidar_xn2000', 'DBH') |>
  dplyr::slice(19) |>
  dplyr::select(poly_id)

sf_polygons_all_out <- sf_polygons |>
  dplyr::mutate(
    geometry = geometry + c(500000, 0),
    tururu = paste0("out_", 1:5)
  ) |>
  sf::st_set_crs(3043)

sf_polygons_one_out <- rbind(sf_polygons, sf_polygons_all_out) |>
  dplyr::slice(1:6)

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
  expect_is(meteolanddb$get_lowres_raster('1981-04-25', 'raster'), 'RasterBrick')
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
  expect_true(
    all(
      names(meteolanddb$get_lowres_raster(start_date, 'stars')) %in%
        c(
          "MeanTemperature", "MinTemperature", "MaxTemperature",
          "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
          "Precipitation", "Radiation", "WindSpeed", "PET", "ThermalAmplitude"
        )
    )
  )
  expect_true(
    all(
      names(meteolanddb$get_lowres_raster('1981-04-25', 'stars')) %in%
        c(
          "MeanTemperature", "MinTemperature", "MaxTemperature",
          "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
          "Precipitation", "Radiation", "WindSpeed", "PET", "ThermalAmplitude"
        )
    )
  )
})

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
  expect_error(
    meteolanddb$points_interpolation(sf_points, c(end_date, start_date)),
    'end date must be equal or more recent'
  )

  expect_s3_class(
    meteolanddb$points_interpolation(sf_points, c(start_date, end_date)),
    'sf'
  )
  # we need an ok interpolation for testing throughfully
  ok_interpolation <-
    meteolanddb$points_interpolation(sf_points, c(start_date, end_date))

  expected_names <- unique(c(
    # from topo:
    'aspect', 'slope', 'elevation',
    # from meteo
    'dates', 'plot_id', 'geometry',
    'MeanTemperature', 'MinTemperature', 'MaxTemperature',
    'MeanRelativeHumidity', 'MinRelativeHumidity', 'MaxRelativeHumidity',
    'Precipitation', 'Radiation', 'WindSpeed', 'PET', 'ThermalAmplitude'
  ))

  expect_equal(nrow(ok_interpolation), 3*5)
  expect_equal(ncol(ok_interpolation), length(expected_names))
  expect_named(ok_interpolation, expected_names, ignore.order = TRUE)

  # if we have elevation on the points, all should work, but without taking the topo from
  # the db. So we will not have aspect and slope as they are missing from the points object
  expect_s3_class(
    suppressWarnings(ok_with_topo_interpolation <- meteolanddb$points_interpolation(
      sf_points_elevation, c(start_date, end_date)
    )),
    "sf"
  )

  expect_equal(nrow(ok_with_topo_interpolation), 3*5)
  expect_equal(ncol(ok_with_topo_interpolation), length(expected_names[3:length(expected_names)]))
  expect_named(ok_with_topo_interpolation, expected_names[3:length(expected_names)], ignore.order = TRUE)


  expect_warning(
    (one_day_missing_interpolation <- meteolanddb$points_interpolation(
      sf_points, c(as.character(Sys.Date() - 2), as.character(Sys.Date() + 1))
    )), "Some dates"
  )

  expect_equal(nrow(one_day_missing_interpolation), 2*5)
  expect_equal(ncol(one_day_missing_interpolation), length(expected_names))
  expect_named(one_day_missing_interpolation, expected_names, ignore.order = TRUE)

  # when all dates are out of range, then error occurs
  expect_error(
    meteolanddb$points_interpolation(
      sf_points, c(historical_start_date, historical_end_date)
    ), "No meteo data found"
  )

  expect_warning(
    (one_coord_missing_interpolation <- meteolanddb$points_interpolation(
      sf_points_one_out, c(start_date, end_date)
    )),
    "Some points"
  )

  expect_equal(nrow(one_coord_missing_interpolation), 3*5)
  expect_equal(ncol(one_coord_missing_interpolation), length(expected_names))
  expect_named(one_coord_missing_interpolation, expected_names, ignore.order = TRUE)

  expect_error(
    meteolanddb$points_interpolation(
      sf_points_all_out, c(start_date, end_date)
    ),
    "All coordinates are not in Catalonia"
  )

  expect_equal(
    meteolanddb$points_interpolation(sf_points, c(start_date, end_date)),
    meteolanddb$points_interpolation(sf_points_3043, c(start_date, end_date))
  )
})

## historical points interpolation works ####
test_that("historical points_interpolation method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(
    meteolanddb$historical_points_interpolation(
      'sf', c(historical_start_date, historical_end_date), 'plot_id'
    ), 'not a simple feature'
  )
  expect_error(
    meteolanddb$historical_points_interpolation(
      sf_points, c(historical_start_date), 'plot_id'
    ), 'must be of length'
  )
  expect_error(
    meteolanddb$historical_points_interpolation(sf_points, c(25, 26), 'plot_id'),
    'not character'
  )
  expect_error(
    meteolanddb$historical_points_interpolation(
      sf_points, c(historical_start_date, historical_end_date), 125
    ), 'not character'
  )
  expect_error(
    meteolanddb$historical_points_interpolation(
      sf_points, c('tururu', 'larara'), 'plot_id'
    ), 'cannot be converted to date'
  )
  expect_error(
    meteolanddb$historical_points_interpolation(
      sf_polygons, c(historical_start_date, historical_end_date), 'plot_id'
    ),
    'is not a POINT'
  )
  expect_error(
    meteolanddb$historical_points_interpolation(
      sf_points, c(historical_end_date, historical_start_date), 'plot_id'
    ), 'end date must be equal or more recent'
  )

  expect_s3_class(
    meteolanddb$historical_points_interpolation(
      sf_points, c(historical_start_date, historical_end_date), 'plot_id'
    ),
    'sf'
  )
  # we need an ok interpolation for testing throughfully
  ok_interpolation <-
    meteolanddb$historical_points_interpolation(
      sf_points, c(historical_start_date, historical_end_date), 'plot_id'
    )

  expected_names <- c(
    'date', 'plot_id', 'geometry',
    'MeanTemperature', 'MinTemperature', 'MaxTemperature',
    'MeanRelativeHumidity', 'MinRelativeHumidity', 'MaxRelativeHumidity',
    'Precipitation', 'Radiation', 'WindSpeed', 'PET', 'ThermalAmplitude'
  )

  expect_equal(nrow(ok_interpolation), 3*5)
  expect_equal(ncol(ok_interpolation), length(expected_names))
  expect_named(ok_interpolation, expected_names, ignore.order = TRUE)


  expect_warning(
    (one_day_missing_interpolation <- meteolanddb$historical_points_interpolation(
      sf_points, c('1975-12-30', '1976-01-01'), 'plot_id'
    )), "Some dates"
  )

  expect_equal(nrow(one_day_missing_interpolation), 5*1)
  expect_equal(ncol(one_day_missing_interpolation), length(expected_names))
  expect_named(one_day_missing_interpolation, expected_names, ignore.order = TRUE)

  # when all dates are out of range, then error occurs
  expect_error(
    suppressWarnings(meteolanddb$historical_points_interpolation(
      sf_points, c(start_date, end_date),
      'plot_id'
    )), "No meteo data found"
  )

  expect_warning(
    (one_coord_missing_interpolation <- meteolanddb$historical_points_interpolation(
      sf_points_one_out, c(historical_start_date, historical_end_date), 'plot_id'
    )),
    "Some points"
  )

  expect_equal(nrow(one_coord_missing_interpolation), 3*6)
  expect_equal(ncol(one_coord_missing_interpolation), length(expected_names))
  expect_named(one_day_missing_interpolation, expected_names, ignore.order = TRUE)

  expect_error(
    suppressWarnings(meteolanddb$historical_points_interpolation(
      sf_points_all_out, c(historical_start_date, historical_end_date), 'plot_id'
    )),
    "All coordinates are not in Catalonia"
  )

  expect_identical(
    meteolanddb$historical_points_interpolation(
      sf_points, c(historical_start_date, historical_end_date), 'plot_id'
    ) |> dplyr::pull(MeanTemperature),
    meteolanddb$historical_points_interpolation(
      sf_points_3043, c(historical_start_date, historical_end_date), 'plot_id'
    ) |> dplyr::pull(MeanTemperature)
  )
})

## raster interolation works ####
test_that("raster_interpolation method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(
    meteolanddb$raster_interpolation('sf', c(start_date, end_date)),
    'not a simple feature'
  )
  expect_error(
    meteolanddb$raster_interpolation(sf_polygons, c(start_date)),
    'must be of length'
  )
  expect_error(
    meteolanddb$raster_interpolation(sf_polygons, c(25, 26)),
    'not character'
  )
  expect_error(
    meteolanddb$raster_interpolation(sf_polygons, c('tururu', 'larara')),
    'cannot be converted to date'
  )
  expect_error(
    meteolanddb$raster_interpolation(sf_points, c(start_date, end_date)),
    'is not a POLYGON'
  )
  expect_error(
    meteolanddb$raster_interpolation(sf_polygons, c(end_date, start_date)),
    'end date must be equal or more recent'
  )
  expect_type(
    meteolanddb$raster_interpolation(sf_polygons, c(start_date, end_date)),
    'list'
  )

  # we need an ok interpolation for testing throughfully
  ok_raster_interpolation <-
    meteolanddb$raster_interpolation(sf_polygons, c(start_date, end_date))

  expect_length(ok_raster_interpolation, 3)
  expect_true(inherits(ok_raster_interpolation[[1]], 'stars'))
  expect_true(
    all(
      names(ok_raster_interpolation[[1]]) %in%
        c(
          "MeanTemperature", "MinTemperature", "MaxTemperature",
          "MeanRelativeHumidity", "MinRelativeHumidity", "MaxRelativeHumidity",
          "Precipitation", "Radiation", "WindSpeed", "PET", "ThermalAmplitude"
        )
    )
  )

  expect_warning(
    (one_day_missing_interpolation <- meteolanddb$raster_interpolation(
      sf_polygons, c(as.character(Sys.Date() - 2), as.character(Sys.Date() + 1))
    )), "Some dates"
  )
  expect_length(one_day_missing_interpolation, 2)
  expect_true(inherits(one_day_missing_interpolation[[1]], 'stars'))

  # when all dates are out of range, then error occurs
  expect_error(
    meteolanddb$raster_interpolation(
      sf_polygons, c(as.character(Sys.Date()), as.character(Sys.Date()))
    ), "No data for the specified dates"
  )

  # expect_warning(
  #   meteolanddb$raster_interpolation(sf_polygons_one_out, c(start_date, end_date)),
  #   "Some polygons"
  # )
  one_coord_missing_interpolation <-
    meteolanddb$raster_interpolation(sf_polygons_one_out, c(start_date, end_date))
  expect_length(one_coord_missing_interpolation, 3)
  expect_true(inherits(one_coord_missing_interpolation[[1]], 'stars'))

  expect_error(
    meteolanddb$raster_interpolation(sf_polygons_all_out, c(start_date, end_date)),
    "No data for the specified dates"
  )

  expect_true(inherits(
    meteolanddb$raster_interpolation(sf_polygons, c('1981-04-24', '1981-04-26'))[[1]],
    'stars'
  ))

})

## external methods work ####
test_that("external get low raster works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    meteolanddb$get_lowres_raster(start_date, 'stars'),
    meteoland_get_lowres_raster(meteolanddb, start_date, 'stars')
  )
  expect_identical(
    meteolanddb$get_lowres_raster(start_date, 'raster'),
    meteoland_get_lowres_raster(meteolanddb, start_date, 'raster')
  )
  expect_error(
    meteoland_get_lowres_raster('meteolanddb', start_date, 'raster'),
    "class lfcMeteoland"
  )
})

test_that("external points interpolation works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    meteolanddb$points_interpolation(
      sf_points, c(start_date, end_date)
    ),
    meteoland_points_interpolation(
      meteolanddb, sf_points, c(start_date, end_date)
    )
  )
  expect_error(
    meteoland_points_interpolation(
      'meteolanddb', sf_points, c(start_date, end_date)
    ),
    "class lfcMeteoland"
  )
})

test_that("external historical points interpolation works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    meteolanddb$historical_points_interpolation(
      sf_points, c(historical_start_date, historical_end_date), 'plot_id'
    ),
    meteoland_historical_points_interpolation(
      meteolanddb, sf_points, c(historical_start_date, historical_end_date),
      'plot_id'
    )
  )
  expect_error(
    meteoland_historical_points_interpolation(
      'meteolanddb', sf_points, c(historical_start_date, historical_end_date),
      'plot_id'
    ),
    "class lfcMeteoland"
  )
})

test_that("external raster interpolation works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    meteolanddb$raster_interpolation(
      sf_polygons, c(start_date, end_date)
    ),
    meteoland_raster_interpolation(
      meteolanddb, sf_polygons, c(start_date, end_date)
    )
  )
  expect_error(
    meteoland_raster_interpolation(
      'meteolanddb', sf_polygons, c(start_date, end_date)
    ),
    "class lfcMeteoland"
  )
})
