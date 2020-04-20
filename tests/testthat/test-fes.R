test_that("class object creation works", {
  expect_is(fes(), c('lfcFES'))
  expect_equal(lfcdata:::lfcFES$new(), fes())
  expect_true(rlang::is_function(fes()$get_data))
  expect_true(rlang::is_function(fes()$avail_tables))
  expect_true(rlang::is_function(fes()$describe_var))
  expect_true(rlang::is_function(fes()$describe_table))
})

# fesdb to avoid calling the db so often
fesdb <- fes()

test_that("get method works", {
  skip_on_cran()
  skip_on_travis()
  expect_s3_class(fesdb$get_data('static', FALSE), 'tbl_df')
  expect_s3_class(fesdb$get_data('static', TRUE), 'sf')
  # errors
  expect_error(
    fesdb$get_data(1, FALSE),
    "not character"
  )
  expect_error(
    fesdb$get_data(c('static', 'plot_ifn_4_results'), FALSE),
    "of length"
  )
  expect_error(
    fesdb$get_data('static', 'FALSE'),
    "not logical"
  )
  expect_error(
    fesdb$get_data('static', NA),
    "is missing"
  )
  expect_error(
    fesdb$get_data('non_existent_table', FALSE),
    "Can not connect to the database:"
  )
})

test_that("avail_tables method works", {
  expect_type(fesdb$avail_tables(), 'character')
  expect_true('static' %in% fesdb$avail_tables())
})

test_that("describe_table method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(fesdb$describe_table('plot_nfi_4_results'), c('lfcFES'))
  expect_output(fesdb$describe_table('plot_nfi_4_results'))
  expect_output(
    fesdb$describe_table(c('plot_nfi_4_results', 'variables_thesaurus'))
  )
  expect_error(
    fesdb$describe_table(c('plot_nfi_4_results', 'tururu')), 'not found'
  )
  expect_error(fesdb$describe_table('tururu'), 'not found')
  expect_error(fesdb$describe_table(25), 'not character')
})

test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(fesdb$describe_var('mushrooms_production'), c('lfcFES'))
  expect_output(fesdb$describe_var('mushrooms_production'))
  expect_output(fesdb$describe_var(c('mushrooms_production', 'exported_water')))
  expect_error(
    fesdb$describe_var(c('mushrooms_production', 'exported_water', 'tururu')),
    'variable not found'
  )
  expect_error(fesdb$describe_var('tururu'), 'variable not found')
  expect_error(fesdb$describe_var(25), 'not character')
})

test_that("cache works", {
  skip_on_cran()
  skip_on_travis()
  expect_length(fesdb$.__enclos_env__$private$data_cache, 4)
  bar <- fesdb$get_data('static', FALSE)
  expect_s3_class(bar, 'tbl_df')
  expect_identical(
    bar,
    dplyr::tbl(fesdb$.__enclos_env__$private$pool_conn, 'static') %>%
      dplyr::collect()
  )
  expect_length(fesdb$.__enclos_env__$private$data_cache, 4)
  baz <- fesdb$get_data('plot_nfi_4_results', FALSE)
  expect_length(fesdb$.__enclos_env__$private$data_cache, 5)
})


rm(fesdb)
