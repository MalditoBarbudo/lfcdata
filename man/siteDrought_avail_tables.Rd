% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lfc_SiteDrought.R
\name{siteDrought_avail_tables}
\alias{siteDrought_avail_tables}
\title{Get the available tables in siteDrought database}
\usage{
siteDrought_avail_tables
}
\arguments{
\item{object}{\code{lfcsiteDrought} object, as created by \code{\link{siteDrought}}}
}
\value{
A character vector with the table names
}
\description{
\code{siteDrought_avail_tables} is a wrapper for the \code{$avail_tables}
   method of \code{lfcsiteDrought} objects. See also \code{\link{siteDrought}}.
}
\examples{

if (interactive()) {

  siteDroughtdb <- lfcdata::siteDrought()
  lfcdata::siteDrought_avail_tables(siteDroughtdb)

  # siteDroughtdb is an R6 object, so the previous examples are the same as:
  siteDroughtdb$avail_tables()

}

}
\seealso{
Other siteDrought functions:
   \code{\link{siteDrought_get_data}} (),
   \code{\link{siteDrought_describe_table}} (),
   \code{\link{siteDrought_describe_var}} (),
   \code{\link{siteDrought}} ()
}
