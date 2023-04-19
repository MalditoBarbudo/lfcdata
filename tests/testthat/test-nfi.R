test_that("class object creation works", {
  expect_is(nfi(), c('lfcNFI'))
  # expect_equal(lfcdata:::lfcNFI$new(), nfi())
  expect_true(rlang::is_function(nfi()$get_data))
  expect_true(rlang::is_function(nfi()$avail_tables))
  expect_true(rlang::is_function(nfi()$describe_var))
  expect_true(rlang::is_function(nfi()$describe_table))
})

# nfidb to avoid calling the db so often
nfidb <- nfi()

test_that("get method works", {
  skip_on_cran()
  skip_on_travis()
  expect_s3_class(nfidb$get_data('plots', FALSE), 'tbl_df')
  expect_s3_class(nfidb$get_data('plots', TRUE), 'sf')
  # errors
  expect_error(
    nfidb$get_data(1, FALSE),
    "not character"
  )
  expect_error(
    nfidb$get_data(c('plots', 'plot_nfi_4_results'), FALSE),
    "of length"
  )
  expect_error(
    nfidb$get_data('plots', 'FALSE'),
    "not logical"
  )
  expect_error(
    nfidb$get_data('plots', NA),
    "is missing"
  )
  expect_error(
    nfidb$get_data('non_existent_table', FALSE),
    "Can not connect to the database:"
  )
})

test_that("avail_tables method works", {
  expect_type(nfidb$avail_tables(), 'character')
  expect_true('plots' %in% nfidb$avail_tables())
})

test_that("describe_table method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(nfidb$describe_table('plot_nfi_4_results'), c('lfcNFI'))
  expect_output(nfidb$describe_table('plot_nfi_4_results'))
  expect_output(nfidb$describe_table(c('plot_nfi_4_results', 'variables_thesaurus')))
  expect_error(nfidb$describe_table(c('plot_nfi_4_results', 'density_dead')), 'not found')
  expect_error(nfidb$describe_table('tururu'), 'not found')
  expect_error(nfidb$describe_table(25), 'not character')
})

test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(nfidb$describe_var('density'), c('lfcNFI'))
  expect_output(nfidb$describe_var('density'))
  expect_output(nfidb$describe_var(c('density', 'density_dead')))
  expect_error(nfidb$describe_var(c('density', 'density_dead', 'tururu')), 'variable not found')
  expect_error(nfidb$describe_var('tururu'), 'variable not found')
  expect_error(nfidb$describe_var(25), 'not character')
})

test_that("cache works", {
  skip_on_cran()
  skip_on_travis()
  expect_length(nfidb$.__enclos_env__$private$data_cache, 4)
  bar <- nfidb$get_data('plots', FALSE)
  expect_s3_class(bar, 'tbl_df')
  expect_identical(
    bar,
    dplyr::tbl(nfidb$.__enclos_env__$private$pool_conn, 'plots') %>%
      dplyr::collect()
  )
  expect_length(nfidb$.__enclos_env__$private$data_cache, 4)
  baz <- nfidb$get_data('plot_nfi_4_results', FALSE)
  expect_length(nfidb$.__enclos_env__$private$data_cache, 5)
})

test_that("external get data wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    nfidb$get_data('plots', FALSE),
    nfi_get_data(nfidb, 'plots', FALSE)
  )
  expect_error(
    nfi_get_data('nfidb', 'plots', FALSE),
    "class lfcNFI"
  )
  xyz <- nfi_get_data(nfidb, 'plot_nfi_3_results', FALSE)
  expect_length(nfidb$.__enclos_env__$private$data_cache, 6)
  expect_identical(
    nfidb$get_data('plot_nfi_3_results', FALSE),
    nfi_get_data(nfidb, 'plot_nfi_3_results', FALSE)
  )
})

test_that("external avail tables wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(nfidb$avail_tables(), nfi_avail_tables(nfidb))
  expect_error(nfi_avail_tables('nfidb'), "class lfcNFI")
})

test_that("external describe_var wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(nfidb$describe_var('density'), nfi_describe_var(nfidb, 'density'))
  expect_error(nfi_describe_var('nfidb', 'density'), "class lfcNFI")
})

test_that("external describe_table wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(nfidb$describe_table('plots'), nfi_describe_table(nfidb, 'plots'))
  expect_error(nfi_describe_table('nfidb', 'density'), "class lfcNFI")
})

rm(nfidb)
