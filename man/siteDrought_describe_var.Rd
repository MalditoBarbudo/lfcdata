% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lfc_SiteDrought.R
\name{siteDrought_describe_var}
\alias{siteDrought_describe_var}
\title{Pint info about the variables present in the siteDrought databases}
\usage{
siteDrought_describe_var(object, variables)
}
\arguments{
\item{object}{\code{lfcsiteDrought} object, as characted by \code{\link{siteDrought}}}

\item{variables}{character vector with the names of the variables to describe}
}
\value{
Description is printed in the console, nothing is returned
}
\description{
\code{siteDrought_describe_var} is a wrapper for the \code{$describe_var} method of
   \code{lfcsiteDrought} objects. See also \code{\link{siteDrought}}.
}
\examples{

if (interactive()) {

   siteDroughtdb <- lfcdata::siteDrought()
   lfcdata::siteDrought_describe_var(siteDroughtdb , 'LFMC_q')
   lfcdata::siteDrought_describe_var(siteDroughtdb , c('DFMC','SFP'))

   # siteDrought is an R6 object, so the previous examples is the same as:
   siteDroughtdb$describe_var('LFMC_q')
   siteDroughtdb$describe_var(c('DFMC','SFP'))

}

}
\seealso{
Other siteDrought functions:
   \code{\link{siteDrought_get_data}} (),
   \code{\link{siteDrought_avail_tables}} (),
   \code{\link{siteDrought_describe_table}} (),
   \code{\link{siteDrought}} ()
}
