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

test_that("description method works", {
  expect_type(foo$description(id = 'BH_287'), 'list')
  expect_type(foo$description(!is.na(independent_var_2)), 'list')
  expect_identical(names(foo$description(id = 'BH_287')), 'BH_287')
  expect_length(foo$description(id = 'BH_287')[[1]], 22)
  expect_true(length(foo$description(!is.na(independent_var_2))) > 1300)
  expect_true('VOB_7674' %in% names(foo$description(!is.na(independent_var_2))))
  expect_error(foo$description(id = 1), 'rlang::is_character')
  expect_error(foo$description(Sys.Date()), 'not evaluate to a logical vector')
})

test_that("calculate method works", {
  expect_type(foo$calculate(DR = c(1,2,3), allometry_id = 'BH_287'), 'double')
  expect_type(foo$calculate(DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'), 'double')
  expect_equal(
    foo$calculate(DR = c(1,2,3), allometry_id = 'BH_287'),
    foo$description(id = 'BH_287')[[1]]$param_a * (c(1,2,3)^foo$description(id = 'BH_287')[[1]]$param_b)
  )
  expect_equal(
    foo$calculate(DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'),
    foo$description(id = 'VOB_7674')[[1]]$param_a + foo$description(id = 'VOB_7674')[[1]]$param_b * (c(1,2,3)*10)^2 * c(10,11,12)
  )
  expect_error(foo$calculate(DR = c(1,2,3), allometry_id = 1), 'rlang::is_character')
  expect_error(foo$calculate(DR = Sys.Date(), allometry_id = 'BH_287'), 'is.numeric')
  expect_error(foo$calculate(c(1,2,3), allometry_id = 'BH_287'), 'stringr::str_length')
})

test_that("describe_var method works", {
  expect_is(foo$describe_var('BR'), c('lfcAllometries'))
  expect_output(foo$describe_var('BR'))
  expect_output(foo$describe_var(c('BR', 'DBH')))
  expect_output(foo$describe_var(c('BR', 'DBH', 'tururu')))
  expect_error(foo$describe_var('tururu'), 'nrow')
  expect_error(foo$describe_var(25), 'rlang::is_character')
})

test_that("cache works", {
  expect_length(foo$.__enclos_env__$private$data_cache, 2)
  bar <- foo$get_data('allometries')
  expect_s3_class(bar, 'tbl_df')
  expect_identical(
    bar,
    dplyr::tbl(foo$.__enclos_env__$private$pool_conn, 'allometries') %>%
      dplyr::collect()
  )
  expect_length(foo$.__enclos_env__$private$data_cache, 2)
  baz <- foo$get_data('thesaurus_variables')
  expect_length(foo$.__enclos_env__$private$data_cache, 3)
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
  expect_length(foo$.__enclos_env__$private$data_cache, 4)
  expect_identical(
    foo$get_data('thesaurus_sources'),
    allometries_get_data(foo, 'thesaurus_sources')
  )
})

test_that("external description wrapper works", {
  expect_identical(
    foo$description(id = 'BH_287'),
    allometries_description(foo, id = 'BH_287')
  )
  expect_identical(
    foo$description(!is.na(independent_var_2)),
    allometries_description(foo, !is.na(independent_var_2))
  )
  expect_error(allometries_description('foo', id = 'BH_287'), "inherits")
  expect_error(allometries_description(foo, id = 1), "rlang::is_character")
})

test_that("external calculate wrapper works", {
  expect_identical(
    foo$calculate(DR = c(1,2,3), allometry_id = 'BH_287'),
    allometries_calculate(foo, DR = c(1,2,3), allometry_id = 'BH_287')
  )
  expect_identical(
    foo$calculate(DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'),
    allometries_calculate(
      foo, DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'
    )
  )
  expect_error(
    allometries_calculate('foo', DR = c(1,2,3), allometry_id = 'BH_287'),
    "inherits"
  )
  expect_error(
    allometries_calculate(
      'foo', DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'
    ),
    "inherits"
  )
  expect_error(
    allometries_calculate(foo, DR = c(1,2,3), allometry_id = 1),
    'rlang::is_character'
  )
})

test_that("external describe_var wrapper works", {
  expect_identical(foo$describe_var('DBH'), allometries_describe_var(foo, 'DBH'))
  expect_error(allometries_describe_var('foo', 'density'), "inherits")
})
