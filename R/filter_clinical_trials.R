#' Filter a set of PMIDs for clinical trials
#' 
#' This function allows the user to filter an already acquired set of PMIDs for those that
#' are associated with a clinical trial, as well as allowing the user to select for FDA-
#' style clinical trials by phase.
#' 
#' @param pmid (vec, char, or list) Either raw output from [query_pubmed] or 
#' one or more PMIDs.
#' 
#' @param phase (int; can be , 1, 2, 3, or 4; default 0) FDA-style clinical trial phase to filter for. 
#' Enter 0 or keep blank to include all clinical trials.
#' 
#' @param ... Arguments to pass to [filter_pub_types].
#' 
#' @return Returns a list object in the same format as [query_pubmed].
#' 
#' @export
filter_clinical_trials <- function(
    pmid, 
    phase = 0,
    ...
    ) {
  if(phase == 0) out <- filter_pub_types(pmid = pmid, pub_type = "Clinical Trial", ...)
  if(phase == 1) out <- filter_pub_types(pmid = pmid, pub_type = "Clinical Trial, Phase I", ...)
  if(phase == 2) out <- filter_pub_types(pmid = pmid, pub_type = "Clinical Trial, Phase II", ...)
  if(phase == 3) out <- filter_pub_types(pmid = pmid, pub_type = "Clinical Trial, Phase III", ...)
  if(phase == 4) out <- filter_pub_types(pmid = pmid, pub_type = "Clinical Trial, Phase IV", ...)
  return(out)
}