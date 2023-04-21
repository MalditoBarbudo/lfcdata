## class object creation works ####
test_that("class object creation works", {
  expect_true(inherits(siteDrought(), c('lfcsiteDrought')))
  expect_true(rlang::is_function(siteDrought()$get_data))
  expect_true(rlang::is_function(siteDrought()$avail_tables))
  expect_true(rlang::is_function(siteDrought()$describe_table))
  expect_true(rlang::is_function(siteDrought()$describe_var))
})

# db object to avoid calling the function so often
sitedroughtdb <- lfcdata::siteDrought()

## get_data method works ####
test_that("get_data method works", {
  skip_on_cran()
  skip_on_travis()

  # expect
  expect_s3_class(sitedroughtdb$get_data(), 'data.frame')
  expect_identical(sitedroughtdb$get_data('data_day_fire'), sitedroughtdb$get_data())

  # errors
  expect_error(sitedroughtdb$get_data(c('data_day','thesaurus_variables_sitedr')), "must be of length")
  expect_error(sitedroughtdb$get_data(FALSE), "Argument table_name is not character")
  expect_error(sitedroughtdb$get_data(21), "Argument table_name is not character")
  expect_error(sitedroughtdb$get_data('random_table'), "Can not connect to the database:")
})


## avail_tables works ####
test_that("avail_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_type(sitedroughtdb$avail_tables(), 'character')
  expect_length(sitedroughtdb$avail_tables(), 1)
  expect_identical(sitedroughtdb$avail_tables(), 'data_day_fire')

  # errors
  expect_error(sitedroughtdb$avail_tables(FALSE), "unused argument")
  expect_error(sitedroughtdb$avail_tables(21), "unused argument")
  expect_error(sitedroughtdb$avail_tables('random_table'), "unused argument")
})


## describe_table works ####
test_that("describe_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_true(inherits(sitedroughtdb$describe_table(), 'lfcsiteDrought'))
  expect_output(sitedroughtdb$describe_table())
  expect_identical(sitedroughtdb$describe_table(), sitedroughtdb$describe_table("data_day_fire"))

  # errors
  expect_error(sitedroughtdb$describe_table(c('data_day_fire','random_table')), "not found")
  expect_error(sitedroughtdb$describe_table('random_table'), 'random_table not found')
  expect_error(sitedroughtdb$describe_table(NA), 'Argument tables is not character')
  expect_error(sitedroughtdb$describe_table(25), 'Argument tables is not character')

})

## describe_var works ####
test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()

  expect_true(inherits(sitedroughtdb$describe_var("REW"), c('lfcsiteDrought')))
  expect_output(sitedroughtdb$describe_var("Precipitation"))
  expect_output(sitedroughtdb$describe_var(c("REW","Precipitation")))

  # errors
  expect_error(sitedroughtdb$describe_var('random'), "not found")
  expect_error(sitedroughtdb$describe_var(c("REW","random")), "not found")
  expect_error(sitedroughtdb$describe_var(NA), "is not character")
  expect_error(sitedroughtdb$describe_var(21), "is not character")
  expect_error(sitedroughtdb$describe_var(), 'is missing')
})
