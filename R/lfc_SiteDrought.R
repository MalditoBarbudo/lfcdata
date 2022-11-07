#'
#'@description \code{siteDrought()} creates and object to access the siteDrought database
#'
#'@title lfcsiteDrought Class
#'
#'@returns An \code{siteDrought} class object (inherits from
#'    \code{\link[R6]{R6Class}}) with methods to access the data.
#'    See Methods section.
#'
#'@section Methods:
#'    \code{lfcsiteDrought} objects has the following public methods.
#'    \itemize{
#'        \item {\code{$get_data}: Retrieve and siteDrought database tables.
#'               See \code{\link{siteDrought_get_data}} for more details.}
#'
#'        \item {\code{$avail_tables}: Retrun a character vector with the names
#'              of the available tables in the database.
#'              See \code{\link{siteDrought_avail_tables}} for more details.}
#'
#'        \item {\code{$describe_table}: Print the information available about the provided table-
#'              See \code{\link{siteDroughtdescribe_table}} for more details.}
#'
#'        \item {\code{$describe_var}: Print information available about the provided variable.
#'              See \code{\link{siteDroughtdescribe_var}} for more details.}
#'    }
#'
#'@seealso Other siteDrought functions:
#'    \code{\link{siteDrought_get_data}} (),
#'    \code{\link{siteDrought_avail_tables}} (),
#'    \code{\link{siteDrought_describe_table}} (),
#'    \code{\link{siteDrought_describe_var}} ()
#'
#'@examples
#'
#'if (interactive()) {
#'
#'   siteDroughtdb <- lfcdata::siteDrought()
#'   siteDroughtdb
#'}
#'
#'@export
siteDrought <- function() {
  lfcsiteDrought$new()
}

lfcsiteDrought <- R6::R6Class(

  classname = "lfcsiteDrought",
  inherit = lfcObject,
  cloneable = FALSE,

  public = list(

    # ................... GET_DATA R ...................
    # ..................................................

    #      .) GET_DATA SIMPRE devuelve tabla (tipo SF) con GEOMETRIA
    #      .) Por lo tanto:

    #             .) NO Usamos SUPER$GET_DATA (devuelve DF sin geometría)
    #             .) CACHE siempre (SPATIAL = TRUE)
    #             .) Usamos el SF::ST_READ  (Se conecta a al BBDD y devuelve SF con geometría)
    #             .) NO Usamos FILTER para seleccionar UNA FECHA
    #                       .) Nos descargamos TODA la TABLA
    #                       .) Con todas las FECHAS y datos
    #                       .) Pesará poco para R y después en la App ya la filtraremos (fecha,variable,...)

    #      .) Tablas por DEFECTO
    #                       .) Usamos (table_name = "data_day_fire") para indicar que el defecto es "data_day_fire"
    #                       .) Así => get_data() lo hace en f(x) de la tabla "data_day_fire"
    #                       .) Y si queremos otra tabla usamos => get_data("otra_tabla")


    get_data = function(table_name = "data_day_fire") {

      check_length_for(table_name, 1, "table_name")
      check_args_for(character = list(table_name = table_name))


      res <- private$data_cache[[glue::glue("{table_name}_TRUE")]] %||%
        {
          message('Querying table from LFC database, this can take a while...')
          query_data_spatial <- sf::st_read(private$pool_conn, table_name)
          message('Done')
          private$data_cache[[glue::glue("{table_name}_TRUE")]] <- query_data_spatial
          query_data_spatial
        }

      return(res)
    },

    # .................. AVAIL TABLES ..................
    # ..................................................

    #      .) El usuario SOLO puede acceder a la tabla DATA_DAY
    #      .) El resto de tablas de la BBDD no son accesibles


    avail_tables = function() {
      c('data_day_fire')
    },

    # ................ DESCRIBE TABLE ..................
    # ..................................................

    #      .) Para obtener VARIABLES THESAURUS
    #      .) Usamos SUPER$GET_DATA
    #      .) Ya que queremos descargar de la BBDD una tabla sin GEOMETRIA
    #      .) Usaremos una tabla Thesaurus especial para el siteDrought


    describe_table = function(tables = "data_day_fire"){

      check_args_for(character = list(tables = tables))
      check_if_in_for(tables, self$avail_tables())

      tables_dict <- siteDrought_table_dictionary()
      variables_thes <- sitedrought_var_thes

      tables %>%
        purrr::map(
          siteDrought_describe_table_cat,
          tables_dict = tables_dict
        )

      return(invisible(self))
    },

    # .................. DESCRIBE VAR ..................
    # ..................................................

    #      .) Obtenemos VARIABLES de la tabla de la BBDD
    #      .) Usamos = SUPER$GET_DATA(VARIABLES THESAURUS SITEDR)

    #      .) Aplicamos función = siteDrought_describe_var_cat
    #      .) Está en el archivo UTILS_SieteDR.R

    describe_var = function(variables) {
      check_args_for(character = list(variables = variables))

      # variables_thes <- suppressMessages(super$get_data('variables_thesaurus_sitedr'))

      variables %>%
        purrr::map(
          siteDrought_describe_var_cat#,
          # variables_thes = variables_thes
        )

      invisible(self)

    },

    # .................... PRINT .......................
    # ..................................................

    #      .) Función solo descriptiva
    #      .) Anuncia y describe las principales funciones

    print = function(...) {
      cat(
        " Access to Laboratori Forestal (CREAF).\n",
        crayon::blue$underline("laboratoriforestal.creaf.cat\n\n"),
        "Use " %+% crayon::yellow$bold("siteDrought_get_data") %+%
          " to access the tables.\n",
        "Use " %+% crayon::yellow$bold("siteDrought_avail_tables") %+%
          " to know which tables are available.\n",
        "Use " %+% crayon::yellow$bold("siteDrought_describe_table") %+%
          " to get the information available on the tables.\n",
        "Use " %+% crayon::yellow$bold("siteDrought_describe_var") %+%
          " to get the information available on the variables.\n"

      )
      invisible(self)
    }

  ),

  private = list(
    dbname ="sitedrought_db"
  )
)

# ........... FUNCIONES de METODOS ..............
# ...............................................

#      .) Estas funciones facilitan al usuario acceder a los métodos
#      .) Cada método tiene una función
#      .) Todas se inicializan con un OBJETO => Es siteDrought()
#      .) CHECKS:
#               .) Solo tienen CHECK CLASS
#               .) Las variables serán Chequeadas por el método
#      .) EJEMPLO de USO:
#               .) sitedr <- lfcdata::sitedrougth()
#               .) siteDrought_avail_tables(sitedr,'data_day')


#'@title Access to the tables in the siteDrought database
#'
#'@description \code{siteDrought_get_data} is a wrapper for the \code{$get_data} method
#'    of \code{lfcsiteDrought} objects. See also \code{\link{siteDrought}}.
#'
#'@usage siteDrought_get_data (object, table_name)
#'
#'@param object \code{lfcsiteDrought} object, as created by \code{\link{siteDrought}}
#'@param table_name character vector of lenght 1 indicating the requested table name
#'
#'@details Connection to database can be slow. Tables retrieved from the db are stored in a temporary cache inside
#'    the lfcsiteDrought object created by \code{\link{siteDrought}}
#'
#'@return A tbl object, sf type
#'
#'@seealso Other siteDrought functions:
#'    \code{\link{siteDrought_avail_tables}} (),
#'    \code{\link{siteDrought_describe_table}} (),
#'    \code{\link{siteDrought_describe_var}} (),
#'    \code{\link{siteDrought}} ()
#'
#'@examples
#'if (interactive()) {
#'
#'   siteDroughtdb <- lfcdata::siteDrought()
#'
#'   # sf tibble
#'   lfcdata::siteDrought_get_data(siteDroughtdb,'data_day')

#'   # we can uses pipes
#'   siteDroughtdb %>%
#'      lfcdata::siteDrought_get_data('data_day')
#'
#'   # siteDroughtdb is an R6 object, so the previous examples are the same as:
#'   siteDroughtdb$get_data('data_day')
#'
#'}
#' @export
siteDrought_get_data <- function(object, table_name = "data_day_fire") {
  check_class_for(object, 'lfcsiteDrought')
  object$get_data(table_name)
}


#'@title Get the available tables in siteDrought database
#'
#'@description \code{siteDrought_avail_tables} is a wrapper for the \code{$avail_tables}
#'    method of \code{lfcsiteDrought} objects. See also \code{\link{siteDrought}}.
#'
#'@usage siteDrought_avail_tables
#'
#'@param object \code{lfcsiteDrought} object, as created by \code{\link{siteDrought}}
#'
#'@return A character vector with the table names
#'
#'@seealso Other siteDrought functions:
#'    \code{\link{siteDrought_get_data}} (),
#'    \code{\link{siteDrought_describe_table}} (),
#'    \code{\link{siteDrought_describe_var}} (),
#'    \code{\link{siteDrought}} ()
#'
#'@examples
#'
#'if (interactive()) {
#'
#'   siteDroughtdb <- lfcdata::siteDrought()
#'   lfcdata::siteDrought_avail_tables(siteDroughtdb)
#'
#'   # siteDroughtdb is an R6 object, so the previous examples are the same as:
#'   siteDroughtdb$avail_tables()
#'
#'}
#'
#' @export
siteDrought_avail_tables <- function(object) {
  check_class_for(object, 'lfcsiteDrought')
  object$avail_tables()
}

#'@title Print info abuout the tables present in the siteDrought database
#'
#'@description \code{siteDrought_describe_table} is a wrapper for the \code{$describe_table} method of
#'    \code{lfcsiteDrought} objects. See also \code{\link{siteDrought}}.
#'
#'@usage siteDrought_describe_table(object, tables)
#'
#'@param object \code{lfcsiteDrought} object, as created by \code{\link{siteDrought}}
#'@param tables character vector with the names of the tables to describe
#'
#'@return Description is printed in the console, nothing is returned
#'
#'
#'@seealso Other siteDrought functions:
#'    \code{\link{siteDrought_get_data}} (),
#'    \code{\link{siteDrought_avail_tables}} (),
#'    \code{\link{siteDrought_describe_var}} (),
#'    \code{\link{siteDrought}} (),
#'
#'@examples
#'
#'if (interactive()) {
#'
#'    siteDroughtdb <- lfcdata::siteDrought()
#'    lfcdata::siteDrought_describe_table(siteDroughtdb , 'data_day')
#'
#'    # siteDrought is an R6 object, so the previus example is the same as:
#'    siteDroughtdb$describe_table('data_day')
#'
#'}
#'
#' @export
siteDrought_describe_table <- function(object, tables) {
  check_class_for(object, 'lfcsiteDrought')
  object$describe_table(tables)
}

#'@title Pint info about the variables present in the siteDrought databases
#'
#'@description \code{siteDrought_describe_var} is a wrapper for the \code{$describe_var} method of
#'    \code{lfcsiteDrought} objects. See also \code{\link{siteDrought}}.
#'
#'@usage siteDrought_describe_var(object, variables)
#'
#'@param object \code{lfcsiteDrought} object, as characted by \code{\link{siteDrought}}
#'@param variables character vector with the names of the variables to describe
#'
#'@return Description is printed in the console, nothing is returned
#'
#'@seealso Other siteDrought functions:
#'    \code{\link{siteDrought_get_data}} (),
#'    \code{\link{siteDrought_avail_tables}} (),
#'    \code{\link{siteDrought_describe_table}} (),
#'    \code{\link{siteDrought}} ()
#'
#'@examples
#'
#'if (interactive()) {
#'
#'    siteDroughtdb <- lfcdata::siteDrought()
#'    lfcdata::siteDrought_describe_var(siteDroughtdb , 'LFMC_q')
#'    lfcdata::siteDrought_describe_var(siteDroughtdb , c('DFMC','SFP'))
#'
#'    # siteDrought is an R6 object, so the previous examples is the same as:
#'    siteDroughtdb$describe_var('LFMC_q')
#'    siteDroughtdb$describe_var(c('DFMC','SFP'))
#'
#'}
#'
#' @export
siteDrought_describe_var <- function(object, variables) {
  check_class_for(object, 'lfcsiteDrought')
  object$describe_var(variables)
}









