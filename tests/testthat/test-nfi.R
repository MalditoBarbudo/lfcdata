test_that("class object creation works", {
  expect_is(nfi(), c('lfcNFI'))
  expect_true(rlang::is_function(nfi()$get_data))
  expect_true(rlang::is_function(nfi()$avail_tables))
  expect_true(rlang::is_function(nfi()$describe_var))
})

# foo to avoid calling the db so ofter
foo <- nfi()

test_that("get method works", {
  expect_s3_class(foo$get_data('plots', FALSE), 'tbl_df')
  expect_s3_class(foo$get_data('plots', TRUE), 'sf')
  # errors
  expect_error(
    foo$get_data(1, FALSE),
    "not character"
  )
  expect_error(
    foo$get_data(c('plots', 'plot_nfi_4_results'), FALSE),
    "of length"
  )
  expect_error(
    foo$get_data('plots', 'FALSE'),
    "not logical"
  )
  expect_error(
    foo$get_data('plots', NA),
    "is missing"
  )
  expect_error(
    foo$get_data('non_existent_table', FALSE),
    "Can not connect to the database:"
  )
})

test_that("avail_tables method works", {
  expect_type(foo$avail_tables(), 'character')
  expect_true('plots' %in% foo$avail_tables())
})

test_that("describe_var method works", {
  expect_is(foo$describe_var('density'), c('lfcNFI'))
  expect_output(foo$describe_var('density'))
  expect_output(foo$describe_var(c('density', 'density_dead')))
  expect_error(foo$describe_var(c('density', 'density_dead', 'tururu')), 'variable not found')
  expect_error(foo$describe_var('tururu'), 'variable not found')
  expect_error(foo$describe_var(25), 'not character')
})

test_that("cache works", {
  expect_length(foo$.__enclos_env__$private$data_cache, 4)
  bar <- foo$get_data('plots', FALSE)
  expect_s3_class(bar, 'tbl_df')
  expect_identical(
    bar,
    dplyr::tbl(foo$.__enclos_env__$private$pool_conn, 'plots') %>%
      dplyr::collect()
  )
  expect_length(foo$.__enclos_env__$private$data_cache, 4)
  baz <- foo$get_data('plot_nfi_4_results', FALSE)
  expect_length(foo$.__enclos_env__$private$data_cache, 5)
})

test_that("external get data wrapper works", {
  expect_identical(
    foo$get_data('plots', FALSE),
    nfi_get_data(foo, 'plots', FALSE)
  )
  expect_error(
    nfi_get_data('foo', 'plots', FALSE),
    "class lfcNFI"
  )
  xyz <- nfi_get_data(foo, 'plot_nfi_3_results', FALSE)
  expect_length(foo$.__enclos_env__$private$data_cache, 6)
  expect_identical(
    foo$get_data('plot_nfi_3_results', FALSE),
    nfi_get_data(foo, 'plot_nfi_3_results', FALSE)
  )
})

test_that("external avail tables wrapper works", {
  expect_identical(foo$avail_tables(), nfi_avail_tables(foo))
  expect_error(nfi_avail_tables('foo'), "class lfcNFI")
})

test_that("external describe_var wrapper works", {
  expect_identical(foo$describe_var('density'), nfi_describe_var(foo, 'density'))
  expect_error(nfi_describe_var('foo', 'density'), "class lfcNFI")
})


