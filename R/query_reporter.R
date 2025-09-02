#' Query NIH RePORTER
#' 
#' This function is a wrapper for the repoRter.nih package's make_req() and
#' get_nih_data() functions. See repoRter.nih documentation for more details.
#' With the exception of fields_help, all param descriptions are copied 
#' directly from the repoRter.nih docs (Copyright (c) 2022 Michael Barr, used
#' under MIT License).
#' 
#' @param criteria list(); the RePORTER Project API query criteria used to filter results (projects).
#'  \strong{See Details for schema and other spec rules.}
#' @param include_fields character(); optional; use to return only the specified fields from the result.
#'  \strong{See Details for valid return field names}
#' @param exclude_fields character(); optional; use to exclude specified fields from the result.
#' @param offset integer(1); optional; default: 0; usually not explicitly passed by user.
#'  Used to set the start index of the results to be retrieved (indexed from 0). \strong{See Details}.
#' @param limit integer(1); optional; default: 500; restrict the number of project records returned per page/request
#'  inside the calling function. Defaulted to the maximum allowed value of 500. Reducing this may help with
#'  bandwidth/timeout issues.
#' @param sort_field character(1); optional; use to sort the result by the specified field.
#'  May be useful in retrieving complete result sets above the API maximum of 10K (but below 2x the max = 20K)
#' @param sort_order character(1): optional; one of "asc" or "desc"; \code{sort_field} must be specified.
#' @param message logical(1); default: TRUE; print a message with the JSON to console/stdout. You may want to
#'  suppress this at times.
#' @param query A valid JSON request formatted for the RePORTER Project API, as returned
#'  by the \code{repoRter.nih::make_req} method
#' @param max_pages numeric(1); default: NULL; An integer specifying to only fetch (up to)
#'  the first \code{max_pages} number of pages from the result set. Useful for testing your
#'  query/obtaining schema information. Default behavior is to fetch all pages.
#' @param flatten_result (default: FALSE) If TRUE, flatten nested dataframes and collapse nested
#'  vectors to a single character column with elements delimited by a semi-colon
#' @param return_meta (default: FALSE) If TRUE, will return a \code{list} containing your result
#'  set as well as the meta data - this includes a count of total projects matching
#'  your query and can be useful for programming.
#' @param help (default: FALSE) If TRUE, opens https://api.reporter.nih.gov which contains
#' information about API fields.
#' 
#' @return When \code{return_meta = FALSE}: a \code{tibble} containing your result set
#'   (up to API max of 10,000 records); else if \code{include_meta = TRUE}, a named list 
#'   containing the result set and the metadata from the initial API response; else if
#'   \code{field_help = TRUE}, a browser page will be opened and no output will be produced.
#'   
#'   If an API error occurs, this method will print an informative message and return \code{NA}.
#' 
#' @export
query_reporter <- function(
    include_fields = NULL, 
    exclude_fields = NULL, 
    offset = 0, 
    limit = 500, 
    sort_field = NULL, 
    sort_order = NULL, 
    message = TRUE, 
    max_pages = NULL,
    flatten_result = TRUE, 
    return_meta = FALSE,
    help = FALSE,
    ...
    ) {
  if(help) {
    browseURL("https://api.reporter.nih.gov")
    return()
  }
  query <- repoRter.nih::make_req(
    criteria = list(...), 
    include_fields = include_fields, 
    exclude_fields = exclude_fields,
    offset = offset,
    limit = limit,
    sort_field = sort_field,
    sort_order = sort_order,
    message = message)
  res <- repoRter.nih::get_nih_data(
    query = query, 
    max_pages = max_pages,
    flatten_result = flatten_result,
    return_meta = return_meta)
}