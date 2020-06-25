## class object creation works ####
test_that("class object creation works", {
  expect_is(lidar(), c('lfcMeteoland'))
  expect_equal(lfcdata:::lfcMeteoland$new(), meteoland())
  expect_true(rlang::is_function(meteoland()$get_data))
  expect_true(rlang::is_function(meteoland()$point_interpolation))
  expect_true(rlang::is_function(meteoland()$raster_interpolation))
})

# lidardb to avoid calling the db so often
meteolanddb <- meteoland()

