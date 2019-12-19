#' @description \code{nfi()} creates an object to access the nfi database.
#'
#' @title lfcNFI class
#'
#' @return An \code{lfcNFI} class object (inherits from \code{\link[R6]{R6Class}}),
#'   with methods to access the data. See Methods section.
#'
#' @section Methods:
#'   \code{lfcNFI} objects has two public methods:
#'   \itemize{
#'     \item{\code{$get_data}: Retrieve and collect NFI database tables. See
#'           \code{\link{nfi_get_data}} for more details}
#'     \item{\code{$avail_tables}: Return a character vector with the names of the
#'           available tables in the database. See \code{\link{nfi_avail_tables}} for
#'           more details}
#'     \item{\code{$describe_var}: Print the information available about the provided
#'           variable. See \code{\link{nfi_describe_var}} for more details}
#'   }
#'
#' @family NFI functions
#'
#' @export
#'
#' @examples
#' nfidb <- nfi()
#' nfidb
nfi <- function() {
  lfcNFI$new()
}

#' @importFrom R6 R6Class
#' @importFrom crayon %+%
lfcNFI <- R6::R6Class(
  # specs
  classname = "lfcNFI",
  inherit = lfcObject,
  cloneable = FALSE,
  # public methods and values
  public = list(
    # get method, modifying the super class method
    get_data = function(table_name, spatial = FALSE) {

      # arguments validation (table name is always validated in the super)
      stopifnot(
        rlang::is_logical(spatial) & !rlang::is_na(spatial)
      )

      res <- private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]] %||%
        {
          # is the query spatial?
          if (!spatial) {
            # if not, return the data as is
            super$get_data(table_name)
          } else {
            # if it is, then convert based on the lat and long vars
            if (all(
              c('coords_longitude', 'coords_latitude') %in% names(super$get_data(table_name))
            )) {
              query_data_spatial <- super$get_data(table_name) %>%
                sf::st_as_sf(
                  coords = c('coords_longitude', 'coords_latitude'), remove = FALSE,
                  crs = 4326
                )
            } else {
              # if there is no lat long vars, then get them from plots
              query_data_spatial <- super$get_data(table_name) %>%
                dplyr::left_join(
                  super$get_data('plots') %>%
                    dplyr::select(plot_id, coords_longitude, coords_latitude) %>%
                    dplyr::collect(),
                  by = 'plot_id'
                ) %>%
                sf::st_as_sf(
                  coords = c('coords_longitude', 'coords_latitude'), remove = FALSE,
                  crs = 4326
                )
            }
            private$data_cache[[glue::glue("{table_name}_{as.character(spatial)}")]] <- query_data_spatial
            query_data_spatial
          }
        }

      return(res)
    },

    # available tables method
    avail_tables = function() {
      dplyr::db_list_tables(private$pool_conn) %>%
        tolower() %>%
        unique()
    },

    # describe variable method
    describe_var = function(variables) {

      # argument checking
      stopifnot(
        rlang::is_character(variables)
      )

      # get the var thes, the numerical var thes filter by the variable and prepare the
      # result with cat, glue and crayon, as a function to apply to a vector of variables
      invisible_cats <- function(variable) {
        no_returned <- self$get_data('variables_thesaurus') %>%
          dplyr::filter(var_id == variable) %>%
          dplyr::left_join(
            self$get_data('variables_numerical'), by = c("var_id", "var_table")
          ) %>%
          dplyr::group_by(var_description_eng) %>%
          dplyr::group_walk(
            ~ cat(
              # var name
              crayon::yellow$bold(glue::glue("{.x$translation_eng %>% unique()} ({.x$var_id %>% unique()})")),
              "\n",
              # tables present
              "Present in the following tables:\n",
              crayon::magenta(
                stringr::str_c("    - ", sort(.x$var_table), collapse = '\n')
              ),
              "\n",
              # var description
              "Description:\n",
              crayon::green(stringr::str_c(
                "    ",
                substring(
                  .y$var_description_eng,
                  seq(1, nchar(.y$var_description_eng), by = 78),
                  seq(78, nchar(.y$var_description_eng) + 78, by = 78)
                ),
                collapse = '\n'
              )),
              "\n",
              # var units
              "Units:  ",
              crayon::blue$bold("[") %+%
                crayon::blue$italic$bold(
                  glue::glue("{(.x$var_units %na% ' - ') %>% unique()}")
                ) %+%
                crayon::blue$bold("]"),
              "\n\n",
              sep = ''
            )
          )
        invisible(variable)
      }

      lapply(variables, invisible_cats)
      invisible(self)

    },

    # override default print
    print = function(...) {
      cat(
        " Access to the Spanish National Forest Inventory data for Catalonia.\n",
        crayon::blue$underline("laboratoriforestal.creaf.uab.cat\n\n"),
        "Use " %+% crayon::yellow$bold("nfi_get_data") %+%
          " to access the tables.\n",
        "Use " %+% crayon::yellow$bold("nfi_avail_tables") %+%
          " to know which tables are available.\n",
        "Use " %+% crayon::yellow$bold("nfi_describe_var") %+%
          " to get the information available on the variables.\n",
        "See " %+%
          crayon::yellow$bold("vignette('tables_and_variables', package = 'lfcdata')") %+%
          " to learn more about the tables and variables."
      )
      invisible(self)
    }
  ),
  # private methods and values
  private = list(
    # connection values
    dbname = 'tururu'
  )
)

#' Access to the tables in the NFI database
#'
#' @description \code{nfi_get_data} is a wrapper for the \code{$get_data} method of
#'   \code{lfcNFI} objects. See also \code{\link{nfi}}.
#'
#' @param object \code{lfcNFI} object, as created by \code{\link{nfi}}
#' @param table_name character vector of lenght 1 indicating the requested table name
#' @param spatial logical indicating if the data must be converted to an spatial object
#'
#' @return A tbl object: tbl_df if spatial is \code{FALSE}, sf if
#'   spatial is \code{TRUE}
#'
#' @family NFI functions
#'
#' @details Connection to database can be slow. Tables retrieved from the db are stored
#'   in a temporary cache inside the lfcNFI object created by \code{\link{nfi}}, making
#'   subsequent calls to the same table are faster.
#'
#' @examples
#' nfidb <- nfi()
#' # tibble
#' nfi_get_data(nfidb, 'plots')
#' # sf tibble
#' nfi_get_data(nfidb, 'plots', TRUE)
#'
#' # we can use pipes
#' nfidb %>%
#'   nfi_get_data('plots', TRUE)
#'
#' # nfidb is an R6 object, so the previous examples are the same as:
#' nfidb$get_data('plots')
#' nfidb$get_data('plots', TRUE)
#'
#' @export
nfi_get_data <- function(object, table_name, spatial = FALSE) {
  # argument validation
  # NOTE: table_name and spatial are validated in the method
  stopifnot(inherits(object, 'lfcNFI'))
  # call to the class method
  object$get_data(table_name, spatial)
}

#' Get the available tables in NFI db
#'
#' @description \code{nfi_avail_tables} is a wrapper for the \code{$avail_tables} method of
#'   \code{lfcNFI} objects. See \code{\link{nfi}}.
#'
#' @param object \code{lfcNFI} object, as created by \code{\link{nfi}}
#'
#' @return A character vector with the table names
#'
#' @family NFI functions
#'
#' @examples
#' nfidb <- nfi()
#' nfi_avail_tables(nfidb)
#'
#' # nfidb is an R6 object, so the previous example is the same as:
#' nfidb$avail_tables()
#'
#' @export
nfi_avail_tables <- function(object) {
  # argument validation
  stopifnot(inherits(object, 'lfcNFI'))
  # call to the class method
  object$avail_tables()
}

#' Print info about the variables present in the NFI db
#'
#' @description \code{nfi_describe_var} is a wrapper for the \code{$describe_var} method of
#'   \code{lfcNFI} objects. See \code{\link{nfi}}.
#'
#' @param object \code{lfcNFI} object, as created by \code{\link{nfi}}
#' @param variables character vector with the names of the variables to describe
#'
#' @return A character vector with the variable names to describe
#'
#' @family NFI functions
#'
#' @examples
#' nfidb <- nfi()
#' nfi_describe_var(nfidb, "density")
#' nfi_describe_var(nfidb, c("over_bark_volume", "basal_area"))
#'
#' # nfidb is an R6 object, so the previous example is the same as:
#' nfidb$describe_var("density")
#' nfidb$describe_var(c("over_bark_volume", "basal_area"))
#'
#' @export
nfi_describe_var <- function(object, variables) {
  # argument validation
  stopifnot(inherits(object, 'lfcNFI'))
  # call to the class method
  object$describe_var(variables)
}
