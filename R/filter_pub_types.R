#' Filter a set of PMIDs for articles that match a specific publication type
#' 
#' This function allows the user to filter an already acquired set of PMIDs for those that
#' match a specific publication type (e.g., "Clinical Trial, Phase I").
#' 
#' @param pmid (vec, char, or list) Either raw output from [query_pubmed] or 
#' one or more PMIDs.
#' 
#' @param pub_type (vec or char) One or more MeSH terms that you would like to include in your results.
#' 
#' @param ... Arguments to pass to [query_pubmed].
#' 
#' @return Returns a list object in the same format as [query_pubmed].
#' 
#' @export
filter_pub_types <- function(
    pmid, 
    pub_type,
    ...
) {
  if(typeof(pmid) == "list") pmid <- pmid$ids
  pmid <- as.character(pmid)
  pmid_str <- paste(pmid, collapse = " OR ")
  pub_type <- paste0(pub_type, "[pt]")
  pub_type_str <- paste(pub_type, collapse = " OR ")
  query <- paste0("(", pmid_str, ") AND (", pub_type_str, ")")
  out <- query_pubmed(query = query, ...)
}