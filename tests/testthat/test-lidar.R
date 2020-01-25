test_that("class object creation works", {
  expect_is(lidar(), c('lfcLiDAR'))
  expect_true(rlang::is_function(lidar()$get_data))
})

# foo to avoid calling the db so often
foo <- lidar()

test_that("get method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(foo$get_data('AB', 'raster'), 'RasterLayer')
  expect_s3_class(foo$get_data('AB', 'stars'), 'stars')
  expect_error(foo$get_data(1, 'raster'), 'not character')
  expect_error(foo$get_data('non_existent_table', 'raster'), 'Must be one of')
  expect_error(foo$get_data('AB', 1), 'not character')
})

test_that("avail_tables method works", {
  expect_type(foo$avail_tables(), 'character')
  expect_true('BAT' %in% foo$avail_tables())
})

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

test_that("cache works", {
  skip_on_cran()
  skip_on_travis()
  expect_length(foo$.__enclos_env__$private$data_cache, 2)
  bar <- foo$get_data('AB', 'raster')
  expect_is(foo$get_data('AB', 'raster'), 'RasterLayer')
  temp_postgresql_conn <- pool::poolCheckout(foo$.__enclos_env__$private$pool_conn)
  expect_identical(
    bar, rpostgis::pgGetRast(temp_postgresql_conn, c('public', 'lidar_stack_utm'), bands = 1)
  )
  pool::poolReturn(temp_postgresql_conn)
  expect_length(foo$.__enclos_env__$private$data_cache, 2)
  baz <- foo$get_data('DBH', 'raster')
  expect_length(foo$.__enclos_env__$private$data_cache, 3)
})

test_that("external get data wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    foo$get_data('AB', 'raster'),
    lidar_get_data(foo, 'AB', 'raster')
  )
  expect_error(
    lidar_get_data('foo', 'AB', 'raster'),
    "class lfcLiDAR"
  )
  xyz <- lidar_get_data(foo, 'REC', 'stars')
  expect_length(foo$.__enclos_env__$private$data_cache, 4)
  expect_identical(
    foo$get_data('REC', 'stars'),
    lidar_get_data(foo, 'REC', 'stars')
  )
})

test_that("external describe_var wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(foo$describe_var('AB'), lidar_describe_var(foo, 'AB'))
  expect_error(lidar_describe_var('foo', 'density'), "class lfcLiDAR")
})
