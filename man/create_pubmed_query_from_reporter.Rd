% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_pubmed_query_from_reporter.R
\name{create_pubmed_query_from_reporter}
\alias{create_pubmed_query_from_reporter}
\title{Create PubMed query based on NIH RePORTER output}
\usage{
create_pubmed_query_from_reporter(
  df,
  terms = NULL,
  start_year = 0,
  end_year = 1,
  proj_title = TRUE,
  proj_terms = TRUE,
  proj_abstract = TRUE,
  verbose = FALSE,
  web_export = FALSE
)
}
\arguments{
\item{df}{(\code{tibble} or \code{data.frame}) Data frame of RePORTER records generated
either from \link{query_reporter} or from creating CSV export file directly from the
RePORTER webpage.}

\item{terms}{(optional; char or vec) One or more terms you would like to filter for in
the project title, terms, and/or abstract. Leave blank to create a PubMed query for
all grants in \code{df}.}

\item{start_year}{(optional; int) Start year for PubMed search. Leave blank or enter 0 to
make this the same as the first fiscal year in \code{df}.}

\item{end_year}{End year for PubMed search. Leave blank or enter 1 to make this the current
year.}

\item{proj_title}{(logical; default = TRUE) Flag to include project title in your filtering.}

\item{proj_terms}{(logical; default = TRUE) Flag to include project terms in your filtering.}

\item{proj_abstract}{(logical; default = TRUE) Flag to include project abstract in your filtering.}

\item{verbose}{(logical; default = FALSE) Print information about filtered grant titles and amounts.}

\item{web_export}{(logical; default = FALSE) Flag to include if you used the CSV export feature of the RePORTER webpage as opposed
to the \link{query_reporter} function.}
}
\value{
Returns a char object containing a valid PubMed query in the format
\code{"(XXX XXXXXXXX[Grants and Funding]) OR (XXX XXXXXXXX[Grants and Funding]) OR ... AND (YYYY:YYYY[pdat])"}
}
\description{
This function creates a PubMed query for all grants in an NIH RePORTER dataset matching
certain conditions.
}
\note{
If using a data frame created by web export, be sure to enable the
\code{web_export} flag!
}
