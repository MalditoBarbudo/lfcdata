test_that("class object creation works", {
  expect_is(allometries(), c('lfcAllometries'))
  # expect_equal(lfcdata:::lfcAllometries$new(), allometries())
  expect_true(rlang::is_function(allometries()$get_data))
  expect_true(rlang::is_function(allometries()$description))
  expect_true(rlang::is_function(allometries()$describe_var))
  expect_true(rlang::is_function(allometries()$calculate))
})

# allomdb to avoid calling the db so often
allomdb <- allometries()

test_that("get method works", {
  skip_on_cran()
  skip_on_travis()
  expect_s3_class(allomdb$get_data('allometries'), 'tbl_df')
  # errors
  expect_error(
    allomdb$get_data(1),
    "not character"
  )
  expect_error(
    allomdb$get_data(c('allometries', 'thesaurus_variables')),
    "of length"
  )
  expect_error(
    allomdb$get_data('non_existent_table'),
    "Can not connect to the database:"
  )
})

test_that("description method works", {
  skip_on_cran()
  skip_on_travis()
  expect_type(allomdb$description(id = 'BH_287'), 'list')
  expect_type(allomdb$description(!is.na(independent_var_2)), 'list')
  expect_identical(names(allomdb$description(id = 'BH_287')), 'BH_287')
  expect_length(allomdb$description(id = 'BH_287')[[1]], 21)
  expect_true(length(allomdb$description(!is.na(independent_var_2))) > 1300)
  expect_true('VOB_7674' %in% names(allomdb$description(!is.na(independent_var_2))))
  expect_error(allomdb$description(id = 1), 'not character')
  expect_error(allomdb$description(Sys.Date()), 'logical')
})

test_that("equation formatter method works", {
  skip_on_cran()
  skip_on_travis()
  eq_test_set <- c(
    "Ht = a·DBH^b", "BFAT = a · PHV^b", "VLE = a + b·(DBH·10) + c·(DBH·10)² + d·(DBH·10)³",
    "VC = a·DBH^b", "VLE = a + b·VOB + c·VOB²", "BST = a·DBH^b",
    "VM = a·DBH^b", "BR = a·(DBH·10)^b", "BAT = a·DBH^b·Ht^c",
    "P_BST = a·BAT^b"
  )
  eq_test_set_latin <- stringr::str_conv(eq_test_set, 'latin1')
  expect_identical(
    allomdb$.__enclos_env__$private$eq_formatter(eq_test_set),
    c(
      "Ht = param_a*DBH^param_b", "BFAT = param_a * PHV^param_b",
      "VLE = param_a + param_b*(DBH*10) + param_c*(DBH*10)^2 + param_d*(DBH*10)^3",
      "VC = param_a*DBH^param_b", "VLE = param_a + param_b*VOB + param_c*VOB^2",
      "BST = param_a*DBH^param_b", "VM = param_a*DBH^param_b", "BR = param_a*(DBH*10)^param_b",
      "BAT = param_a*DBH^param_b*Ht^param_c", "P_BST = param_a*BAT^param_b"
    )
  )
  expect_identical(
    allomdb$.__enclos_env__$private$eq_formatter(eq_test_set_latin),
    c(
      "Ht = param_a*DBH^param_b", "BFAT = param_a * PHV^param_b",
      "VLE = param_a + param_b*(DBH*10) + param_c*(DBH*10)^2 + param_d*(DBH*10)^3",
      "VC = param_a*DBH^param_b", "VLE = param_a + param_b*VOB + param_c*VOB^2",
      "BST = param_a*DBH^param_b", "VM = param_a*DBH^param_b", "BR = param_a*(DBH*10)^param_b",
      "BAT = param_a*DBH^param_b*Ht^param_c", "P_BST = param_a*BAT^param_b"
    )
  )
})

test_that("calculate method works", {
  skip_on_cran()
  skip_on_travis()
  expect_type(allomdb$calculate(DR = c(1,2,3), allometry_id = 'BH_287'), 'double')
  expect_type(allomdb$calculate(DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'), 'double')
  expect_equal(
    allomdb$calculate(DR = c(1,2,3), allometry_id = 'BH_287'),
    allomdb$description(id = 'BH_287')[[1]]$param_a * (c(1,2,3)^allomdb$description(id = 'BH_287')[[1]]$param_b)
  )
  expect_equal(
    allomdb$calculate(DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'),
    allomdb$description(id = 'VOB_7674')[[1]]$param_a + allomdb$description(id = 'VOB_7674')[[1]]$param_b * (c(1,2,3)*10)^2 * c(10,11,12)
  )
  expect_error(allomdb$calculate(DR = c(1,2,3), allometry_id = 1), 'not character')
  expect_error(allomdb$calculate(DR = Sys.Date(), allometry_id = 'BH_287'), 'not numeric')
  expect_error(
    allomdb$calculate(c(1,2,3), allometry_id = 'BH_287'),
    'DR'
  )
  # errors/warnings expected when bad variable names supplied
  expect_error(
    allomdb$calculate(DB = c(1,2,3), allometry_id = 'BH_287'),
    'DR'
  )
  expect_error(
    allomdb$calculate(DC = c(1,2,3), DB = c(1,2,3), allometry_id = 'BH_287'),
    'DR'
  )
  expect_warning(
    allomdb$calculate(DR = c(1,2,3), DB = c(1,2,3), allometry_id = 'BH_287'),
    'DB'
  )
  expect_warning(
    allomdb$calculate(DC = c(1,2,3), DR = c(1,2,3), DB = c(1,2,3), allometry_id = 'BH_287'),
    'DC, DB'
  )
  suppressWarnings(expect_equal(
    allomdb$calculate(DR = c(1,2,3), DB = c(1,2,3), allometry_id = 'BH_287'),
    allomdb$calculate(DR = c(1,2,3), allometry_id = 'BH_287')
  ))
})

test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(allomdb$describe_var('BR'), c('lfcAllometries'))
  expect_output(allomdb$describe_var('BR'))
  expect_output(allomdb$describe_var(c('BR', 'DBH')))
  expect_output(allomdb$describe_var(c('BR', 'DBH', 'tururu')))
  expect_error(allomdb$describe_var('tururu'), 'variables not found')
  expect_error(allomdb$describe_var(25), 'not character')
})

test_that("cache works", {
  skip_on_cran()
  skip_on_travis()
  expect_length(allomdb$.__enclos_env__$private$data_cache, 2)
  bar <- allomdb$get_data('allometries')
  expect_s3_class(bar, 'tbl_df')
  expect_identical(
    bar,
    dplyr::tbl(allomdb$.__enclos_env__$private$pool_conn, 'allometries') |>
      dplyr::collect()
  )
  expect_length(allomdb$.__enclos_env__$private$data_cache, 2)
  baz <- allomdb$get_data('thesaurus_variables')
  expect_length(allomdb$.__enclos_env__$private$data_cache, 3)
})

test_that("external get data wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    allomdb$get_data('allometries'),
    allometries_get_data(allomdb, 'allometries')
  )
  expect_error(
    allometries_get_data('allomdb', 'allometries'),
    "class lfcAllometries"
  )
  xyz <- allometries_get_data(allomdb, 'thesaurus_sources')
  expect_length(allomdb$.__enclos_env__$private$data_cache, 4)
  expect_identical(
    allomdb$get_data('thesaurus_sources'),
    allometries_get_data(allomdb, 'thesaurus_sources')
  )
})

test_that("external description wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    allomdb$description(id = 'BH_287'),
    allometries_description(allomdb, id = 'BH_287')
  )
  expect_identical(
    allomdb$description(!is.na(independent_var_2)),
    allometries_description(allomdb, !is.na(independent_var_2))
  )
  expect_error(allometries_description('allomdb', id = 'BH_287'), "class lfcAllometries")
  expect_error(allometries_description(allomdb, id = 1), "not character")
})

test_that("external calculate wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    allomdb$calculate(DR = c(1,2,3), allometry_id = 'BH_287'),
    allometries_calculate(allomdb, DR = c(1,2,3), allometry_id = 'BH_287')
  )
  expect_identical(
    allomdb$calculate(DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'),
    allometries_calculate(
      allomdb, DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'
    )
  )
  expect_error(
    allometries_calculate('allomdb', DR = c(1,2,3), allometry_id = 'BH_287'),
    "class lfcAllometries"
  )
  expect_error(
    allometries_calculate(
      'allomdb', DBH = c(1,2,3), Ht = c(10,11,12), allometry_id = 'VOB_7674'
    ),
    "class lfcAllometries"
  )
  expect_error(
    allometries_calculate(allomdb, DR = c(1,2,3), allometry_id = 1),
    'not character'
  )
})

test_that("external describe_var wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(allomdb$describe_var('DBH'), allometries_describe_var(allomdb, 'DBH'))
  expect_error(allometries_describe_var('allomdb', 'density'), "class lfcAllometries")
})
