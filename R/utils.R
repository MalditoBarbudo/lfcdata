## syntactic sugars ####
# syntactic sugar for "one or another if one is null" case. taken from tidyverse utils
`%||%` <- function(a, b) {
  if (rlang::is_null(a)) b else a
}

# syntactic sugar for "one or another if one is na" case.
`%na%` <- function(a, b) {
  if (rlang::is_na(a)) b else a
}

## argument and other checks ####
# argument checking
check_args_for <- function(
  character = NULL, numerical = NULL, logical = NULL, na = NULL,
  sf = NULL, points = NULL, polygons = NULL, date = NULL
) {

  # characters
  if (!rlang::is_null(character)) {
    not_complying <- character |>
      purrr::map(rlang::is_character) |>
      purrr::keep(.p = ~!isTRUE(.x)) |>
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not character\n"
      )
      stop(error_message)
    }
  }

  # numerical
  if (!rlang::is_null(numerical)) {
    not_complying <- numerical |>
      purrr::map(rlang::is_bare_numeric) |>
      purrr::keep(.p = ~!isTRUE(.x)) |>
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "One or more variables are not numeric\n"
      )
      stop(error_message)
    }
  }

  # logical
  if (!rlang::is_null(logical)) {
    not_complying <- logical |>
      purrr::map(rlang::is_logical) |>
      purrr::keep(.p = ~!isTRUE(.x)) |>
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not logical\n"
      )
      stop(error_message)
    }
  }

  # na
  if (!rlang::is_null(na)) {
    not_complying <- na |>
      purrr::map(rlang::is_na) |>
      purrr::keep(.p = ~isTRUE(.x)) |>
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is missing\n"
      )
      stop(error_message)
    }
  }

  # sf
  if (!rlang::is_null(sf)) {
    not_complying <- sf |>
      purrr::map(inherits, what = 'sf') |>
      purrr::keep(.p = ~ !isTRUE(.x)) |>
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not a simple feature (sf)\n"
      )
      stop(error_message)
    }
  }

  # points
  if (!rlang::is_null(points)) {
    not_complying <- points |>
      purrr::map(
        ~ all(sf::st_is(.x, type = 'POINT'))
        # sf::st_is, type = 'POINT'
      ) |>
      purrr::keep(.p = ~ !isTRUE(.x)) |>
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} is not a POINT (sf)\n"
      )
      stop(error_message)
    }
  }

  # polygons
  if (!rlang::is_null(polygons)) {
    not_complying <- polygons |>
      purrr::map(
        .f = function(x) {
          all(sf::st_is(x, c('POLYGON', 'MULTIPOLYGON')))
        }
      ) |>
      purrr::keep(.p = ~ !isTRUE(.x)) |>
      names()
    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} ",
        "is not a POLYGON or a MULTIPOLYGON (sf)\n"
      )
      stop(error_message)
    }
  }

  # dates
  if (!rlang::is_null(date)) {
    not_complying <- date |>
      purrr::map(
        .f = function(x) {
          date_check <- try(as.Date(x))
          if (is(date_check, 'try-error')) {
            return(FALSE)
          }
          # if x is a vector, with valid and invalid dates as characters,
          # as.Date is going to return NAs for invalid ones, check for that too:
          if (any(is.na(date_check))) {
            return(FALSE)
          }
          return(TRUE)
        }
      ) |>
      purrr::keep(.p = ~ !isTRUE(.x)) |>
      names()

    if (length(not_complying) > 0) {
      error_message <- glue::glue(
        "Argument {glue::glue_collapse(not_complying)} ",
        "cannot be converted to date"
      )
      stop(error_message)
    }
  }
}

# check for class
check_class_for <- function(object, class) {
  if (!inherits(object, class)) {
    error_message <- glue::glue("object is not from class {class}")
    stop(error_message)
  }
}

# check for length
check_length_for <- function(object, expected_length, arg_name = 'object') {
  if (length(object) != expected_length) {
    error_message <- glue::glue("{arg_name} must be of length {expected_length}")
    stop(error_message)
  }
}

# check if in a character vector
check_if_in_for <- function(object, in_template) {
  if (any(!object %in% in_template)) {
    not_complying <- which(!object %in% in_template)
    allowed_entries <- glue::glue_collapse(in_template, sep = ', ', width = 120)
    error_message <- glue::glue(
      "{object[not_complying]} not found. Must be one of {allowed_entries}"
      # "object must be one of {allowed_entries}, not '{object}'"
    )
    stop(error_message)
  }
}

# check if filter result is at least one row
check_filter_for <- function(object, error_message) {
  if (nrow(object) < 1) {
    stop(error_message)
  }
  invisible(object)
}

## dictionaries and lookup tables ####
nfi_table_dictionary <- function() {
  c(
    plot = 'Plots eco-physiological variables',
    species = 'Plots eco-physiological variables broken down by species',
    simpspecies = 'Plots eco-physiological variables broken down by simplified species',
    genus = 'Plots eco-physiological variables broken down by genus',
    dec = 'Plots eco-physiological variables broken down by deciduous/esclerophyill/conifers',
    bc = 'Plots eco-physiological variables broken down by broadleaf and conifers',

    diamclass = ', also broken down by diameter classes',

    comp = ' from the comparision between',

    nfi2 = ' NFI version 2 (1990-1991)',
    nfi3 = ' NFI version 3 (2000-2001)',
    nfi4 = ' NFI version 4 (2013-2016)',

    '2' = ' from NFI version 2 (1990-1991)',
    '3' = ' from NFI version 3 (2000-2001)',
    '4' = ' from NFI version 4 (2013-2016)',

    plots = 'Plots general info',
    dynamic = 'dynamic (meaning it changes for each NFI version)',

    shrub = 'Plots eco-physiological variables broken down by shrub species',
    regeneration = 'Plots eco-physiological variables for small trees broken down by species',

    variables = 'Thesaurus for variables',
    numerical = ' (limits and units for numerical variables)'
  )
}

fes_table_dictionary <- function() {
  c(
    plot = 'Forest ecosystem service variables',

    comp = ' from the comparision between',

    nfi2 = ' NFI version 2 (1990-1991)',
    nfi3 = ' NFI version 3 (2000-2001)',
    nfi4 = ' NFI version 4 (2013-2016)',

    '2' = ' corresponding to the NFI version 2 (1990-1991)',
    '3' = ' corresponding to the NFI version 3 (2000-2001)',
    '4' = ' corresponding to the NFI version 4 (2013-2016)',

    variables = 'Thesaurus for variables',
    static = 'Static forest ecosystem service data'
  )
}

## cats functions (describe_* helpers) ####
nfi_describe_table_cat <- function(table, tables_dict, variables_thes) {

  # variables present in the table (courtesy of variables_thesaurus)
  variable_names <- variables_thes |>
    dplyr::filter(.data$var_table == table) |>
    dplyr::pull(.data$var_id) |>
    unique()

  # table name deconstructed to query the nfi table dictionary
  table_deconstructed <- stringr::str_split(table, '_') |>
    purrr::flatten_chr()

  ## cats
  # table name
  cat('\n', crayon::yellow$bold(table), '\n', sep = '')
  # table description
  cat(
    glue::glue("{tables_dict[table_deconstructed] |> purrr::discard(is.na)}") |>
      glue::glue_collapse() |>
      crayon::green() |>
      strwrap(width = 75),
    # '\n',
    fill = 80, sep = ''
  )
  # table variables
  cat('Variables in table:\n')
  cat(
    glue::glue(" - {sort(variable_names)}") |>
      glue::glue_collapse(sep = '\n') |>
      crayon::magenta()
  )
  cat('\n')

  # return nothing (invisible NULL)
  return(invisible(NULL))
}

nfi_describe_var_cat <- function(variable, variables_thes, numerical_thes) {

  # trick to use "." without CRAN note
  . <- NULL

  # get the var thes, the numerical var thes filter by the variable and
  # prepare the result with cat, glue and crayon, as a function to apply to
  # a vector of variables.
  variables_thes |>
    dplyr::filter(.data$var_id == variable) |>
    check_filter_for(glue::glue("{variable} variable not found")) |>
    dplyr::left_join(numerical_thes, by = c("var_id", "var_table")) |>
    dplyr::group_by(.data$var_description_eng) |>
    dplyr::group_walk(
      ~ cat(
        "\n",
        # var name
        crayon::yellow$bold(glue::glue(
          "{.x$translation_eng |> unique()} ({.x$var_id |> unique()})"
        )),
        "\n",
        # var description
        strwrap(crayon::green(.y$var_description_eng), width = 72),
        "\n",
        # var units
        crayon::blue$bold(
          "Units: [" %+%
            crayon::blue$italic$bold(
              glue::glue("{(.x$var_units %na% ' - ') |> unique()}")
            ) %+%
            "]"
        ),
        "\n",
        # tables present
        "Present in the following tables:\n",
        crayon::magenta(glue::glue_collapse(
          glue::glue(" - {sort(.x$var_table)}"), sep = '\n'
        )),
        # cat options
        sep = '', fill = 80
      )
    )
  # return nothing (invisible NULL)
  return(invisible(NULL))
}

allometries_describe_var_cat <- function(variables, thes) {

  # trick to use "." withou CRAN note
  . <- NULL

  no_returned <- thes |>
    dplyr::filter(.data$text_id %in% variables) |>
    check_filter_for(glue::glue("one or more variables not found")) |>
    dplyr::group_by(.data$translation_eng) |>
    dplyr::group_walk(
      ~ cat(
        # var name
        crayon::yellow$bold(stringr::str_split_fixed(.y$translation_eng, ' \\(', 2)[1]),
        "\n",
        # var units
        "Units:  ",
        crayon::blue$bold("[") %+%
          crayon::blue$italic$bold(
            glue::glue("{(.x$var_units %na% ' - ') |> unique()}")
          ) %+%
          crayon::blue$bold("]"),
        "\n",
        "English abbreviation:  ",
        crayon::blue$italic$bold(
          glue::glue("{(.x$var_abbr_eng %na% ' - ') |> unique()}")
        ),
        "\n",
        "Data abbreviation:  ",
        crayon::blue$italic$bold(
          glue::glue("{(.x$var_abbr_spa %na% ' - ') |> unique()}")
        ),
        "\n\n",
        sep = ''
      )
    )
  return(invisible(NULL))
}

lidar_describe_var_cat <- function(variables, thes) {
  no_returned <- thes |>
    dplyr::filter(.data$var_id %in% variables) |>
    dplyr::collect() |>
    dplyr::group_by(.data$var_id) |>
    dplyr::group_walk(
      ~ cat(
        # var name
        crayon::yellow$bold(.x$translation_eng),
        "\n",
        # var units
        "Units:  ",
        crayon::blue$bold("[") %+%
          crayon::blue$italic$bold(
            glue::glue("{(.x$var_units %na% ' - ') |> unique()}")
          ) %+%
          crayon::blue$bold("]"),
        "\n",
        "Details:  ",
        crayon::blue$italic$bold(
          glue::glue("{(.x$var_description_eng %na% ' - ') |> unique()}")
        ),
        "\n\n",
        sep = ''
      )
    )
  return(invisible(NULL))
}

fes_describe_table_cat <- function(table, tables_dict, variables_thes) {

  # variables present in the table (courtesy of variables_thesaurus)
  variable_names <- variables_thes |>
    dplyr::filter(.data$var_table == table) |>
    dplyr::pull(.data$var_id) |>
    unique()

  # table name deconstructed to query the nfi table dictionary
  table_deconstructed <- stringr::str_split(table, '_') |>
    purrr::flatten_chr()

  ## cats
  # table name
  cat('\n', crayon::yellow$bold(table), '\n', sep = '')
  # table description
  cat(
    glue::glue("{tables_dict[table_deconstructed] |> purrr::discard(is.na)}") |>
      glue::glue_collapse() |>
      crayon::green() |>
      strwrap(width = 75),
    # '\n',
    fill = 80, sep = ''
  )
  # table variables
  cat('Variables in table:\n')
  cat(
    glue::glue(" - {sort(variable_names)}") |>
      glue::glue_collapse(sep = '\n') |>
      crayon::magenta()
  )
  cat('\n')

  # return nothing (invisible NULL)
  return(invisible(NULL))
}

fes_describe_var_cat <- function(variable, variables_thes) {

  # trick to use "." without CRAN note
  . <- NULL

  # get the var thes, the numerical var thes filter by the variable and
  # prepare the result with cat, glue and crayon, as a function to apply to
  # a vector of variables.
  variables_thes |>
    dplyr::filter(.data$var_id == variable) |>
    check_filter_for(glue::glue("{variable} variable not found")) |>
    dplyr::group_by(.data$var_description_eng) |>
    dplyr::group_walk(
      ~ cat(
        "\n",
        # var name
        crayon::yellow$bold(glue::glue(
          "{.x$translation_eng |> unique()} ({.x$var_id |> unique()})"
        )),
        "\n",
        # var description
        strwrap(crayon::green(.y$var_description_eng), width = 72),
        "\n",
        # var units
        crayon::blue$bold(
          "Units: [" %+%
            crayon::blue$italic$bold(
              glue::glue("{(.x$var_units %na% ' - ') |> unique()}")
            ) %+%
            "]"
        ),
        "\n",
        # tables present
        "Present in the following tables:\n",
        crayon::magenta(glue::glue_collapse(
          glue::glue(" - {sort(.x$var_table)}"), sep = '\n'
        )),
        # cat options
        sep = '', fill = 80
      )
    )
  # return nothing (invisible NULL)
  return(invisible(NULL))
}

catdrought_describe_var_cat <- function(variables) {

  # tibble with the variables thesaurus
  # variable thesaurus
  variable_thesaurus <- tibble::tribble(
    # headers
    ~var_id, ~var_units,
    ~translation_eng, ~translation_spa, ~translation_cat,
    ~var_description_eng, ~var_description_spa, ~var_description_cat,
    # values
    'DDS', '0-1',
    "Intensitat de l'estrès", "Stress intensity", "Intensidad del estrés",
    "", "", "",
    'DeepDrainage', "mm",
    "Drenatje a profunditat", "Deep drainage", "Drenaje a profundidad",
    "", "", "",
    'Eplant', "mm",
    "Transpiració", "Plant transpiration", "Transpiración plantas",
    "", "", "",
    'Esoil', "mm",
    "Evaporació del sòl", "Soil evaporation", "Evaporación del suelo",
    "", "", "",
    'Interception', "mm",
    "Intercepció", "Interception", "Intercepción",
    "", "", "",
    'Precipitation', "mm",
    "Precipitació", "Precipitation", "Precipitación",
    "", "", "",
    'Infiltration', "mm",
    "Infiltració", "Infiltration", "Infiltración",
    "", "", "",
    'LAI', "m2/m2",
    "Índex d'àrea foliar", "Leaf area index", "Índice de área foliar",
    "", "", "",
    'PET', "mm",
    "Evapo-transpiració potencial", "Potential evapo-transpiration", "Evap-transpiración potencial",
    "", "", "",
    'Psi', "MPa",
    "Potencial hídric del sòl", "Soil water potential", "Potencial hídrico del suelo",
    "", "", "",
    'REW', "0-1",
    "Aigua extraïble relativa", "Relative extractable water", "Agua extraible relativa",
    "", "", "",
    'Runoff', "mm",
    "Escolament", "Run-off", "Escorrentía",
    "", "", "",
    'Theta', "m3/m3",
    "Contingut d'humitat", "Soil moisture content", "Contenido de humedad del suelo",
    "", "", "",
    "LMFC", "%",
    "Contingut d'humitat de el combustible viu", "Live Fuel Moisture Content", "Contenido de humedad del combustible vivo",
    "", "", "",
  )

  no_returned <- variable_thesaurus |>
    dplyr::filter(.data$var_id %in% variables) |>
    dplyr::group_by(.data$var_id) |>
    dplyr::group_walk(
      ~ cat(
        # var name
        crayon::yellow$bold(.x$translation_eng),
        "\n",
        # var units
        "Units:  ",
        crayon::blue$bold("[") %+%
          crayon::blue$italic$bold(
            glue::glue("{(.x$var_units %na% ' - ') |> unique()}")
          ) %+%
          crayon::blue$bold("]"),
        "\n",
        "Details:  ",
        crayon::blue$italic$bold(
          glue::glue("{(.x$var_description_eng %na% ' - ') |> unique()}")
        ),
        "\n\n",
        sep = ''
      )
    )
  return(invisible(NULL))
}


# ...... SITEDROUGHT TABLE DICTIONARY .......
# ...........................................

#        .) Tenemos solo UNA TABLA => data_day
#        .) El diccionario es :
#              .) DATA
#              .) DAY


siteDrought_table_dictionary <- function() {
  c(
    day = ', every day are generated variables from MEDFATE Model.',
    data = 'Plots eco-physiological variables from NFI version 4 (2013-2016) of Catalunya, Scrub monitoring plots (MatoSeg project, CTFC),
    permanent forest plots of the Aigüestortes and Estany de Sant Maurici National Park and
    Plots of the National Forest Inventory (IFN3) of the Ordesa y Monteperdido National Park'
  )
}


# ........... DESCRIBRE TABLE ...............
# ...........................................

#        .) VARIABLE_NAMES:
#              .) De la tabla VARIABLE THESAURUS nos quedamos
#              .) Con las variable que la COLUMNA VAR_Table es = ala TABLA
#              .) El PULL hace que nos quedamos SOLO con la columna VAR_ID (REW,PET,LAI,...)
#              .) El UNIQUE nos evita las repeticiones

#        .) TABLE_DECONSTRUCTED:
#              .) Hacemos SPLIT (`_') con el nombre de la TABLA
#              .) Después cada parte las compararemos con el diccionario
#              .) Así tendremos una DESCRIPCION de la TABLA

#              .) O sea, para DESCRIVIR la TABLA usamos la palabras de la misma Tabla
#              .) Y el comparar-lo con un diccionario para describir-lo

#              .) Lo podría hacer con una CONSULTA SQL a una SUB TABLA
#              .) Pero esto implicaría conectar con la BBDD y esto tardaría mas tiempo

#        .) CAT:
#              .) TABLES_DICT[table_deconstructed]
#                     .) De la definciones del DICCIONARIO nos quedamos SOLO
#                     .) Con las que coinciden con las palabras del SPLIT TABLE
#              .) SORT(variable_names)
#                     .) Ordenamos la VARIABLE_NAME


siteDrought_describe_table_cat = function(table, tables_dict){

  variable_names <- sitedrought_var_thes |>
    dplyr::filter(.data$var_table == table) |>
    dplyr::pull(.data$var_id) |>
    unique()

  table_deconstructed <- stringr::str_split(table, '_') |>
    purrr::flatten_chr()


  cat('\n', crayon::yellow$bold(table), '\n', sep = '')
  # table descriptio
  cat(
    glue::glue("{tables_dict[table_deconstructed] |> purrr::discard(is.na)}") |>
      glue::glue_collapse() |>
      crayon::green() |>
      strwrap(width = 75),
    # '\n',
    fill = 80, sep = ''
  )

  cat('Variables in table:\n')
  cat(
    glue::glue(" - {sort(variable_names)}") |>
      glue::glue_collapse(sep = '\n') |>
      crayon::magenta()
  )
  cat('\n')
}

# .......... DESCRIBRE VARIABLE .............
# ...........................................

#        .) VARIABLE_THES:
#              .) De la tabla VARIABLE THESAURUS
#              .) Seleccionamos VAR_ID = VARIABLE
#              .) Aplicamos un CAT

#        .) CAT:
#              .) AMARILLO = Descripción (VARIABLE)
#              .) VERDE = Descripción en inglés de la Variable
#              .) AZUL = Unidades
#              .) GRIS = Table


siteDrought_describe_var_cat <- function(variable) {

    sitedrought_var_thes |>
    dplyr::filter(.data$var_id == variable) |>
    check_filter_for(glue::glue("{variable} variable not found")) |>
    dplyr::group_walk(
      ~ cat(
        "\n",
        # var name
        crayon::yellow$bold(glue::glue(
          "{.x$var_description_eng |> unique()} ({.x$var_id |> unique()})"
        )),
        "\n",
        # var description
        strwrap(crayon::green(.x$var_description_help_eng), width = 72),
        "\n",
        # var units
        crayon::blue$bold(
          "Units:" %+%
            crayon::blue$italic$bold(
              glue::glue("{(.x$var_units_eng %na% ' - ') |> unique()}")
            )
        ),
        "\n",
        # tables present
        "Present in the following tables:\n",
        crayon::magenta(glue::glue_collapse(
          glue::glue(" - {sort(.x$var_table)}"), sep = '\n'
        )),
        # cat options
        sep = '', fill = 80
      )
    )



  return(invisible(NULL))
}

## postgis raster functions ####

get_raster_from_db <- function(
  conn, table_name, rast_column = "rast", bands = TRUE, clip = NULL
) {

  ## assertions
  check_args_for(
    character = list(table_name = table_name, rast_column = rast_column)
  )

  ## db checks
  if (!all(c(
    # valid pool
    pool::dbIsValid(conn),
    # table exists
    pool::dbExistsTable(conn, table_name)
  ))) {
    stop("Connection to DB invalid or table missing")
  }

  ## bands
  band_index_query <- glue::glue_sql(
    "SELECT ST_NumBands({`rast_column`})
      FROM {`table_name`}
      WHERE {`rast_column`} IS NOT NULL LIMIT 1;",
    .con = conn
  )

  db_band_index <- 1:pool::dbGetQuery(conn, band_index_query)[,1]

  band_names_query <- glue::glue_sql(
    "SELECT DISTINCT band_names as band_names
      FROM {`table_name`};",
    .con = conn
  )

  db_band_names <- try({pool::dbGetQuery(conn, band_names_query)[["band_names"]]})

  if (inherits(db_band_names, "try-error")) {
    db_band_names <- letters[db_band_index]
  } else {
    db_band_names <- db_band_names |>
      as.character() |>
      stringr::str_remove_all(pattern = "[{|}]*") |>
      stringr::str_split(pattern = ',', simplify = FALSE) |>
      magrittr::extract2(1)
  }


  if (!rlang::is_logical(bands)) {
    # check if is numeric
    check_args_for(numerical = list(bands = bands))

    # check if provided bands are in the db raster bands range
    check_if_in_for(bands, db_band_index)

    db_band_index <- db_band_index[bands]
    db_band_names <- db_band_names[bands]

  }

  ## SRID (to get the crs for later)
  srid_query <- glue::glue_sql(
    "SELECT DISTINCT (ST_SRID({`rast_column`}))
    FROM {`table_name`}
    WHERE {`rast_column`} IS NOT NULL;",
    .con = conn
  )

  raster_srid <- pool::dbGetQuery(conn, srid_query)[["st_srid"]]

  if (length(raster_srid) > 1) {
    stop("Raster table has more than one SRID")
  } else if (length(raster_srid) < 1) {
    stop("Raster table is empty")
  }
  # query to get the custom ref for debugging purposes
  # pool::dbGetQuery(
  #   conn,
  #   glue::glue_sql("SELECT * FROM spatial_ref_sys WHERE srid = {raster_srid}")
  # )

  # historical rasters have custom srids, why? i don't know but for now we get this
  # shorted here:
  if (raster_srid == 880001) {
    # raster_srid <- 3043
    raster_srid <- pool::dbGetQuery(
      conn,
      glue::glue_sql("SELECT * FROM spatial_ref_sys WHERE srid = {raster_srid}")
    )$proj4text
  }

  ## alignment
  # Should I check the aligment??? Not for now

  ## clip
  # default query, is the end of the info and values queries that come later
  clip_subquery <- glue::glue_sql(") as a", .con = conn)
  # if clip is an sf, the query needs a WHERE with the intersection with the polygon
  if (!rlang::is_null(clip)) {

    #### TODO: What to do when clip has more than one polgyon:
    ####    - option 1: union all (I go with this one)
    ####    - option 2: filter the first (Nope)
    ####    - non an option: looping through polygons and returning a raster for each polygon

    # check that clip is an sf and a polygon
    check_args_for(
      sf = list(clip = clip),
      polygons = list(clip = clip)
    )
    # get the poly wkt
    polygon_ewkt <- clip |>
      sf::st_union() |>
      sf::st_transform(crs = raster_srid) |>
      sf::st_geometry() |>
      sf::st_as_text(EWKT = TRUE)

    # if crs is 88* (custom), we need to add a fix to the intersect
    if (is.character(raster_srid)) {
      polygon_ewkt <- glue::glue(
        "SRID=880001;{polygon_ewkt}"
      )
    }

    # build the subquery
    clip_subquery <- glue::glue_sql(
      " WHERE ST_Intersects({`rast_column`}, ST_GeomFromEWKT({polygon_ewkt}))) as tururu",
      .con = conn
    )
  }

  ## raster info for later calling terra::rast
  query_info <- glue::glue_sql(
    "SELECT
        ST_XMax(ST_Envelope(rast)) as xmax,
        ST_XMin(ST_Envelope(rast)) as xmin,
        ST_YMax(ST_Envelope(rast)) as ymax,
        ST_YMin(ST_Envelope(rast)) as ymin,
        ST_Width(rast) as ncols,
        ST_Height(rast) as nrows
    FROM
        (SELECT ST_Union({`rast_column`}) rast
        FROM {`table_name`}{clip_subquery};",
    .con = conn
  )

  raster_info <- pool::dbGetQuery(conn, query_info)

  # raster values
  unnest_subquery <- glue::glue_sql(
    "unnest(ST_DumpValues(rast, {db_band_index})) as {`db_band_names`}", .con = conn
  ) |>
    glue::glue_sql_collapse(sep = ", ")

  query_values <- glue::glue_sql(
    "SELECT {unnest_subquery}
    FROM
        (SELECT ST_UNION({`rast_column`}) rast
        FROM {`table_name`}{clip_subquery};",
    .con = conn
  )

  raster_values <- pool::dbGetQuery(conn, query_values)

  if (all(is.na(raster_values))) {
    stop("No values found in the raster table")
  }

  ## build the raster object
  # we use the first band to build the base raster
  res <- terra::rast(
    nrows = raster_info$nrows, ncols = raster_info$ncols,
    xmin = raster_info$xmin, xmax = raster_info$xmax,
    ymin = raster_info$ymin, ymax = raster_info$ymax,
    crs = sf::st_crs(raster_srid)[["wkt"]],
    vals = raster_values[, db_band_names[1]],
    names = db_band_names[1]
  )
  # and if there is more bands, then we add them to the raster
  if (length(db_band_index) > 1) {
    for (index in 2:length(db_band_index)) {
      res[[db_band_names[index]]] <- raster_values[, db_band_names[index]]
    }
  }

  return(res)
}
