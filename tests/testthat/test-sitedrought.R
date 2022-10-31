
# .......... INCIALIZAR .............
# ...................................

#       .) Inicializar Clase

sitedrdb <- lfcdata::siteDrought()

# ...... FUNCIONES PRINCIPALES ......
# ...................................

#       .) Testar las principles funciones

test_that("class object creation works", {
  expect_is(lfcdata::siteDrought(), c('lfcsiteDrought'))
  expect_equal(lfcdata:::lfcsiteDrought$new(), siteDrought())
  expect_true(rlang::is_function(sitedrdb$get_data))
  expect_true(rlang::is_function(sitedrdb$avail_tables))
  expect_true(rlang::is_function(sitedrdb$describe_table))
  expect_true(rlang::is_function(sitedrdb$describe_var))

})


# ........... GET DATA ..............
# ...................................

#       .) Test Get_Data Method

test_that("get_data method works", {

  # Si las pruebas deben ejecutarse con relativa rapidez,
  # idealmente menos de un minuto en total. Utilice skip_on_cran()
  # en una prueba que sea inevitablemente de larga duración.

  skip_on_cran()
  skip_on_travis()

  # expect
  expect_s3_class(sitedrdb$get_data('data_day'),'data.frame')

  # errors
  expect_error(sitedrdb$get_data(c('data_day','thesaurus_variables_sitedr')),
               "Expecting a single string value")
  expect_error(sitedrdb$get_data(FALSE),
               "Argument table_name is not character")
  expect_error(sitedrdb$get_data(21),
               "Argument table_name is not character")
  expect_error(sitedrdb$get_data('random_table'),
               "no existe la relación «random_table»")

}
)


# .......... AVAIL TABLE ............
# ...................................

#       .) Test Avail Table Method

test_that("avail_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(sitedrdb$avail_tables(), c('character'))
  expect_type(sitedrdb$avail_tables(),'character')
  expect_true("data_day" %in% sitedrdb$avail_tables())

  # errors
  expect_error(sitedrdb$avail_tables(FALSE),
               "unused argument")
  expect_error(sitedrdb$avail_tables(21),
               "unused argument")
  expect_error(sitedrdb$avail_tables('random_table'),
               "unused argument")
}
)


# ......... DESCRIBE TABLE ..........
# ...................................

#       .) Test Describe Table Method

test_that("describe_table method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(sitedrdb$describe_table("data_day"), c('lfcsiteDrought'))
  expect_output(sitedrdb$describe_table("data_day"))

  # errors
  expect_error(sitedrdb$describe_table(c('data_day','random_table')),
               "not found")
  expect_error(sitedrdb$describe_table('random_table'),
               'random_table not found')
  expect_error(sitedrdb$describe_table(NA),
               'Argument tables is not character')
  expect_error(sitedrdb$describe_table(25),
               'Argument tables is not character')
  expect_error(sitedrdb$describe_table(),
               "el argumento \"tables\" está ausente, sin valor por omisión")

}
)


# ........ DESCRIBE VARIABLE ........
# ...................................

#       .) Test Describe Table Method


test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()

  expect_is(sitedrdb$describe_var("REW"),c('lfcsiteDrought'))
  expect_output(sitedrdb$describe_var("Precipitation"))
  expect_output(sitedrdb$describe_var(c("REW","Precipitation")))

  # errors
  expect_error(sitedrdb$describe_var('random'),
               "not found")
  expect_error(sitedrdb$describe_var(c("REW","random")),
               "not found")
  expect_error(sitedrdb$describe_var(NA),
               "Argument variables is not character")
  expect_error(sitedrdb$describe_var(21),
               Message = "Argument variables is not character")
  expect_error(sitedrdb$describe_var(),
               Message = 'argument "variables" is missing')
})

rm(sitedrdb)


# %%%%%%%%%%%%%%%%%   AKI MHE KEDAAAAT  %%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# .............. INFO TESTING ..............
# ..........................................

#    .) https://r-pkgs.org/testing-basics.html





