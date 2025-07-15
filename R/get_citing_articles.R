#' Query PubMed
#' 
#' This function uses \code{rentrez::entrez_link} 
#' function to find citations of specified article(s). See \code{rentrez} 
#' documentation for more details.
#' 
#' @param pmid (char, vec, or list) Either raw output from [query_pubmed], or a vector of one or more 
#' PMIDs that you want to find citations for, or a single PMID.
#' 
#' @return A vector of PMIDs corresponding to articles that cite the provided PMID(s).
#' 
#' @export
get_citing_articles <- function(
    pmid
    ) {
  if(typeof(pmid) == "list") pmid <- pmid$ids
  pmid <- as.character(pmid)
  links <- rentrez::entrez_link(dbfrom = "pubmed", db = "pubmed", id = pmid)
  return(links$links$pubmed_pubmed_citedin)
}