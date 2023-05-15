## postgis raster functions ####
is_postgis_db <- function(conn) {
  check_postgis <- pool::dbGetQuery(
    conn,
    "SELECT name FROM pg_available_extensions WHERE name = 'postgis';"
  ) |>
    nrow()

  return(check_postgis == 1L)
}

## bs
# Return indexes for an exact number of blocks for a raster
#
# Taken from original code from rpostgis library
#
# @param r a raster
# @param blocks Number of desired blocks (columns, rows)
bs <- function(r, blocks) {

  # ensure blocks are integers
  blocks <- as.integer(blocks)

  # blocks checks
  # must be numbers no na with length 1 or 2
  if (any(is.na(blocks)) || length(blocks) > 2) {
    stop("blocks must be a 1- or 2-length integer vector")
  }
  # must not be 0
  if (any(blocks == 0)) {
    stop("blocks must be greater than zero (0)")
  }
  # if length 1, the create the 2-length vector
  if (length(blocks) == 1) {
    blocks <- rep(blocks, 2)
  }

  # prepare raster
  if (!inherits(r, "SpatRaster")) {
    stop("r must be a SpatRaster object")
  }
  # we only need one layer for this
  r <- r[[1]]

  # prepare results objects with default 1 block
  cr <- list(row = 1, nrows = terra::ncol(r), n = 1)
  tr <- list(row = 1, nrows = terra::nrow(r), n = 1)

  # cr
  b <- blocks[1]
  n.r <- terra::ncol(r)
  if (b != 1) {
    if (b >= n.r) {
      b <- n.r
    }

    if (n.r %% b == 0) {
      by <- n.r/b
      cr$row <- seq(1, to = n.r, by = by)
      cr$nrows <- rep(by, b)
      cr$n <- length(cr$row)
    } else {
      by <- floor(n.r/b)
      cr$row <- c(1, seq(1 + by + (n.r %% b), to = n.r, by = by))
      cr$nrows <- c(cr$row[2:length(cr$row)], n.r + 1) - cr$row
      cr$n <- length(cr$row)
    }
  }

  # tr
  b <- blocks[2]
  n.r <- terra::nrow(r)
  if (b != 1) {
    if (b >= n.r) {
      b <- n.r
    }

    if (n.r %% b == 0) {
      by <- n.r/b
      tr$row <- seq(1, to = n.r, by = by)
      tr$nrows <- rep(by, b)
      tr$n <- length(tr$row)
    } else {
      by <- floor(n.r/b)
      tr$row <- c(1, seq(1 + by + (n.r %% b), to = n.r, by = by))
      tr$nrows <- c(tr$row[2:length(tr$row)], n.r + 1) - tr$row
      tr$n <- length(tr$row)
    }
  }

  return(list(cr = cr, tr = tr))
}

# read raster
get_raster_from_db <- function(
    conn, table_name, rast_column = "rast", bands = TRUE, clip = NULL
) {

  ## assertions
  check_args_for(
    character = list(table_name = table_name, rast_column = rast_column)
  )

  ## db checks
  if (!is_postgis_db(conn)) {
    stop("PostGIS extension not available in connected database")
  }

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

  ####
  # DEBUG!!
  # query to get the custom ref for debugging purposes
  # pool::dbGetQuery(
  #   conn,
  #   glue::glue_sql("SELECT * FROM spatial_ref_sys WHERE srid = {raster_srid}")
  # )
  ####

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

    #### What to do when clip has more than one polgyon:
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

# write raster
write_raster_to_db <- function(raster_obj, conn, table_name, blocks = NULL, .overwrite = FALSE, .append = FALSE) {

  ## assertions
  check_args_for(
    character = list(table_name = table_name)
  )

  ## db checks
  # valid conn
  if (!pool::dbIsValid(conn)) {
    stop("Connection to DB invalid or table missing")
  }
  # postgis extension
  if (!is_postgis_db(conn)) {
    stop("PostGIS extension not available in connected database")
  }

  ## raster checks
  if (!any(inherits(raster_obj, "stars"), inherits(raster_obj, "SpatRaster"))) {
    stop("raster object must be of class 'stars' or 'SpatRaster'")
  }
  # # convert to stars if terra to work more comfortably
  # if (inherits(raster_obj, "SpatRaster")) {
  #   raster_obj <- stars::st_as_stars(raster_obj, ignore_file = TRUE, as_attributes = TRUE)
  # }
  # convert to terra if stars to work more comfortably
  if (inherits(raster_obj, "stars")) {
    original_names <- names(raster_obj)
    raster_obj <- terra::rast(raster_obj)
    names(raster_obj) <- original_names
  }

  # drop table if overwrite
  if (.overwrite) {
    drop_query <- glue::glue_sql(
      "DROP TABLE IF EXISTS {`table_name`};",
      .con = conn
    )
    pool::dbExecute(conn, drop_query)
  }

  # create table if it not exists or has been dropped
  if (!pool::dbExistsTable(conn, table_name)) {
    create_table_query <- glue::glue_sql(
      "CREATE TABLE {`table_name`} (rid serial primary key, band_names text[], rast raster);",
      .con = conn
    )
    pool::dbExecute(conn, create_table_query)
    n_base <- 0
    .append <- FALSE
  } else {
    if (!.append) {
      stop("If want to append to an existing table, '.append = TRUE' must be used.")
    }
    # append not implemented
    stop("'.append = TRUE' not implemented yet. For now tables must be dropped")
  }

  # get the raster crs, in numeric form
  raster_srid <- terra::crs(raster_obj, describe = TRUE)$code |> as.numeric()
  # check if there is a crs and exit if not
  if (length(raster_srid) < 1) {
    stop("raster object must have a valid crs assigned")
  }

  # get the raster resolution
  raster_resolution <- terra::res(raster_obj) |> round(10)

  # bit depth
  bit_depth <- "32BF"
  raster_values <- terra::values(raster_obj)
  if (is.integer(raster_values)) {
    if (min(raster_values, na.rm = TRUE) >= 0) {
      bit_depth <- "32BUI"
    } else {
      bit_depth <- "32BSI"
    }
  }

  # no_data value
  no_data_val <- -99999

  # band names
  # band_names <- glue::glue_collapse(names(raster_obj), sep = "}},{{")
  band_names <- glue::glue("{{{names(raster_obj)}}}") |>
    glue::glue_collapse(sep = ',')
  band_names_array <- glue::glue("{{{band_names}}}")
  band_names_subquery <- glue::glue_sql(
    "{band_names_array}",
    .con = conn
  )

  # check raster crs exists in postgis database
  check_crs_query <- glue::glue_sql(
    "SELECT srid FROM spatial_ref_sys WHERE auth_name = 'EPSG' AND auth_srid = {raster_srid};",
    .con = conn
  )
  check_crs <- pool::dbGetQuery(conn, check_crs_query) |>
    nrow()
  if (check_crs < 1) {
    stop("raster CRS is not in the database spatial_ref_sys")
  }

  # block size
  if (is.null(blocks)) {
    warning("no blocks provided, using one single block. This can cause errors when writing big rasters to the database")
    blocks <- 1
  }
  blocks_size <- bs(raster_obj, blocks)

  message("Splitting ",length(names(raster_obj))," band(s) into ", blocks_size$cr$n, " x ", blocks_size$tr$n, " blocks...")

  # loop by bands for writing the values
  for (band_index in 1:length(names(raster_obj))) {

    # get raster with the band
    raster_band <- raster_obj[[band_index]]

    # rid counter
    rid_index <- n_base

    # # convert NA values to ndval
    # raster_matrix <- terra::as.matrix(raster_band, wide = TRUE)
    # raster_matrix[is.na(raster_matrix)] <- no_data_val

    for (block_index_t in 1:blocks_size$tr$n) {
      raster_band_t <- raster_band[
        blocks_size$tr$row[block_index_t]:(blocks_size$tr$row[block_index_t] + blocks_size$tr$nrows[block_index_t] - 1), , drop = FALSE
      ]

      for (block_index_c in 1:blocks_size$cr$n) {

        raster_band_t_c <- raster_band_t[
          , blocks_size$cr$row[block_index_c]:(blocks_size$cr$row[block_index_c] + blocks_size$cr$nrows[block_index_c] - 1), drop = FALSE
        ]

        # raster extent & dimension
        raster_extent <- terra::ext(raster_band_t_c)
        raster_dimensions <- dim(raster_band_t_c[[1]])
        # rid counter
        rid_index <- rid_index + 1
        message("Writing block ", rid_index, " for band ", band_index)

        # if is the first band, we need to prepare the table
        if (band_index == 1L) {
          # make an empty raster
          # ST_MakeEmptyRaster needs:
          #   - width, which are the columns, so dim[2]
          #   - height, which are the rows, so dim[1]
          #   - upperleftx, which is the x coord of the top left corner, so ext[1]
          #   - upperlefty, which is the y coord of the top left corner, so ext[4]
          #   - scalex, which is the resolution in x, so res[1],
          #   - scaley, which is the resoluction in y, so -res[2],
          #   - skewx and skewy, which are 0
          #   - srid of the raster, so raster_srid
          create_empty_raster_query <- glue::glue_sql(
            "INSERT INTO {`table_name`} (rid, band_names, rast)
            VALUES ({rid_index}, {band_names_subquery},
              ST_MakeEmptyRaster({raster_dimensions[2]}, {raster_dimensions[1]}, {raster_extent[1]}, {raster_extent[4]}, {raster_resolution[1]}, {-raster_resolution[2]}, 0, 0, {raster_srid[1]}));",
            .con = conn
          )
          pool::dbExecute(conn, create_empty_raster_query)

          # upperleft for alignment snapping if needed
          upper_left_x <- pool::dbGetQuery(
            conn,
            glue::glue_sql("SELECT ST_UpperLeftX(rast) x FROM {`table_name`} where rid = 1;", .con = conn)
          ) |> as.numeric()
          upper_left_y <- pool::dbGetQuery(
            conn,
            glue::glue_sql("SELECT ST_UpperLeftY(rast) x FROM {`table_name`} where rid = 1;", .con = conn)
          ) |> as.numeric()

          # add new band
          snap_to_grid <- glue::glue_sql(") WHERE rid = {rid_index}", .con = conn)
          if (raster_resolution[1] != raster_resolution[2]) {
            snap_to_grid <- glue::glue_sql(", {raster_resolution[1]}, {-raster_resolution[2]}) WHERE rid = {rid_index}", .con = conn)
          }

          band_arguments <- glue::glue_sql("ROW({1:length(names(raster_obj))}, {bit_depth}::text,0,{no_data_val})", .con = conn) |>
            glue::glue_sql_collapse(sep = ',')

          snap_band_query <- glue::glue_sql(
            "UPDATE {`table_name`}
            SET rast = ST_SnapToGrid(ST_AddBand(rast, ARRAY[{band_arguments}]::addbandarg[]), {upper_left_x}, {upper_left_y}{snap_to_grid};",
            .con = conn
          )
          pool::dbExecute(conn, snap_band_query)
        }

        # convert NA values to ndval
        raster_matrix <- terra::as.matrix(raster_band_t_c, wide = TRUE)
        raster_matrix[is.na(raster_matrix)] <- no_data_val

        values_array <- purrr::map_chr(
          1:nrow(raster_matrix),
          \(row_index) {glue::glue_sql("[{glue::glue_sql_collapse(raster_matrix[row_index,], sep = ',')}]", .con = conn)}
        ) |>
          glue::glue_sql_collapse(sep = ',')
        update_values_query <- glue::glue_sql(
          "UPDATE {`table_name`}
          SET rast = ST_SetValues(rast, {band_index}, 1, 1, ARRAY[{values_array}]::double precision[][])
          WHERE rid = {rid_index};",
          .con = conn
        )
        pool::dbExecute(conn, update_values_query)

      }
    }
  }

  # create index
  create_index_query <- glue::glue_sql(
    "CREATE INDEX {`glue::glue('{table_name}_rast_st_conhull_idx')`} ON {`table_name`} USING gist( ST_ConvexHull(rast) );",
    .con = conn
  )
  pool::dbExecute(conn, create_index_query)

  # add constraints
  constraints_query <- glue::glue_sql(
    "SELECT AddRasterConstraints({table_name}::name, 'rast'::name)",
    .con = conn
  )
  pool::dbExecute(conn, constraints_query)

  return(invisible(raster_obj))
}

