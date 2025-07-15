#' Filter a set of PMIDs for articles that include a set of MeSH terms
#' 
#' This function allows the user to filter an already acquired set of PMIDs for those that
#' are associated with one or more specific MeSH terms (e.g., "Clinical Trials, Phase I as Topic")
#' 
#' @param pmid (vec, char, or list) Either raw output from [query_pubmed] or 
#' one or more PMIDs.
#' 
#' @param mesh_term (vec or char) One or more MeSH terms that you would like to include in your results.
#' 
#' @param ... Arguments to pass to [query_pubmed].
#' 
#' @return Returns a list object in the same format as [query_pubmed].
#' 
#' @export
filter_mesh_terms <- function(
    pmid, 
    mesh_term,
    ...
    ) {
  if(typeof(pmid) == "list") pmid <- pmid$ids
  pmid <- as.character(pmid)
  pmid_str <- paste(pmid, collapse = " OR ")
  mesh_term <- paste0(mesh_term, "[MeSH]")
  mesh_term_str <- paste(mesh_term, collapse = " OR ")
  query <- paste0("(", pmid_str, ") AND (", mesh_term_str, ")")
  out <- query_pubmed(query = query, ...)
}