#' Query PubMed
#' 
#' This function uses the \code{rentrez::entrez_search} 
#' function to search PubMed. See \code{rentrez} documentation for more details. The only 
#' significant change is that 
#' 
#' @param query (char) This is your PubMed query. Outputs from \code{create_pubmed_query}
#' can go here.
#' 
#' @param retmax (int) Maximum number of records to be returned, set by default to the max that 
#' the PubMed API will allow.
#' 
#' @param unlist (logical, default = TRUE) Lets the user change the \code{$ids} attribute of the 
#' output from a \code{list} to \code{vec} object. Enabled by default for quality of life.
#' 
#' @param ... Additional arguments accepted by the \code{entrez_search} function, should you wish
#' to include them.
#' 
#' @return A \code{list} object with information about the results (including PMIDs).
#' 
#' @export
query_pubmed <- function(
    query, 
    retmax = 9999, 
    unlist = TRUE,
    ... 
    ) {
  out <- rentrez::entrez_search(db = "pubmed", term = query, retmax = retmax, ... )
  out$ids <- unlist(out$ids)
  return(out)
}