test_that("class object creation works", {
  expect_is(allometries(), c('lfcAllometries', 'R6'))
  expect_true(rlang::is_function(allometries()$get_data))
})

# foo to avoid calling the db so often
foo <- allometries()

test_that("get method works", {
  expect_s3_class(foo$get_data('allometries'), 'tbl_df')
  # errors
  expect_error(
    foo$get_data(1),
    "rlang::is_character"
  )
  expect_error(
    foo$get_data(c('allometries', 'thesaurus_variables')),
    "length\\(table_name\\)"
  )
  expect_error(
    foo$get_data('non_existent_table'),
    "Can not connect to the database:"
  )
})

test_that("cache works", {
  expect_length(foo$.__enclos_env__$private$data_cache, 1)
  bar <- foo$get_data('allometries')
  expect_s3_class(bar, 'tbl_df')
  expect_identical(
    bar,
    dplyr::tbl(foo$.__enclos_env__$private$pool_conn, 'allometries') %>%
      dplyr::collect()
  )
  expect_length(foo$.__enclos_env__$private$data_cache, 1)
  baz <- foo$get_data('thesaurus_variables')
  expect_length(foo$.__enclos_env__$private$data_cache, 2)
})

test_that("external get data wrapper works", {
  expect_identical(
    foo$get_data('allometries'),
    allometries_get_data(foo, 'allometries')
  )
  expect_error(
    allometries_get_data('foo', 'allometries'),
    "inherits"
  )
  xyz <- allometries_get_data(foo, 'thesaurus_sources')
  expect_length(foo$.__enclos_env__$private$data_cache, 3)
  expect_identical(
    foo$get_data('thesaurus_sources'),
    allometries_get_data(foo, 'thesaurus_sources')
  )
})
