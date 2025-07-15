#' Create PubMed query based on NIH RePORTER output
#' 
#' This function creates a PubMed query for all grants in an NIH RePORTER dataset matching 
#' certain conditions.
#' 
#' @param df (\code{tibble} or \code{data.frame}) Data frame of RePORTER records generated 
#' either from [query_reporter] or from creating CSV export file directly from the 
#' RePORTER webpage.
#' 
#' @note If using a data frame created by web export, be sure to enable the 
#' \code{web_export} flag!
#' 
#' @param terms (optional; char or vec) One or more terms you would like to filter for in 
#' the project title, terms, and/or abstract. Leave blank to create a PubMed query for 
#' all grants in \code{df}.
#' 
#' @param start_year (optional; int) Start year for PubMed search. Leave blank or enter 0 to 
#' make this the same as the first fiscal year in \code{df}.
#' 
#' @param end_year End year for PubMed search. Leave blank or enter 1 to make this the current 
#' year.
#' 
#' @param proj_title (logical; default = TRUE) Flag to include project title in your filtering.
#' 
#' @param proj_terms (logical; default = TRUE) Flag to include project terms in your filtering.
#' 
#' @param proj_abstract (logical; default = TRUE) Flag to include project abstract in your filtering.
#' 
#' @param verbose (logical; default = FALSE) Print information about filtered grant titles and amounts.
#' 
#' @param web_export (logical; default = FALSE) Flag to include if you used the CSV export feature of the RePORTER webpage as opposed 
#' to the [query_reporter] function.
#' 
#' @return Returns a char object containing a valid PubMed query in the format 
#' \code{"(XXX XXXXXXXX[Grants and Funding]) OR (XXX XXXXXXXX[Grants and Funding]) OR ... AND (YYYY:YYYY[pdat])"}
#' 
#'
#' @export
create_pubmed_query_from_reporter <- function(
    df, 
    terms = NULL, 
    start_year = 0, 
    end_year = 1, 
    proj_title = TRUE, 
    proj_terms = TRUE, 
    proj_abstract = TRUE, 
    verbose = FALSE, 
    web_export = FALSE
    ) {
  if(web_export) {
    df <- df |> 
      dplyr::mutate(project_serial_num = paste0(IC, Serial.Number)) |> 
      dplyr::mutate(activity_code = Activity) |> 
      dplyr::mutate(fiscal_year = Fiscal.Year) |> 
      dplyr::mutate(award_amount = Total.Cost) |> 
      dplyr::mutate(project_title = Project.Title) |> 
      dplyr::mutate(pref_terms = Project.Terms) |> 
      dplyr::mutate(abstract_text = Project.Abstract)
  }
  df <- df |> 
    dplyr::mutate(pubmed_search_term = paste0("(", activity_code, " ", project_serial_num, "[Grants and Funding])")) |> 
    dplyr::mutate(project_title = tolower(project_title)) |> 
    dplyr::mutate(pref_terms = tolower(pref_terms)) |> 
    dplyr::mutate(abstract_text = tolower(abstract_text)) |> 
    dplyr::mutate(matching_field = rep("", nrow(df)))
  if(!is.null(terms)) {
    terms_regex <- paste0("(?<![:alnum:])", terms, "(?![:alnum:])") #look up stringr regex to see what this does (basically, term is not flanked by either a letter or a number)
    terms_str <- paste(tolower(terms_regex), collapse="|")
    if(proj_title) df <- dplyr::mutate(df, matching_field = paste0(df$matching_field, " ", df$project_title))
    if(proj_terms) df <- dplyr::mutate(df, matching_field = paste0(df$matching_field, " ", df$pref_terms))
    if(proj_abstract) df <- dplyr::mutate(df, matching_field = paste0(df$matching_field, " ", df$abstract_text))
    df <- dplyr::filter(df, stringr::str_detect(matching_field, terms_str))
    if(verbose) {
      print(paste0(nrow(df), " projects found with terms ", paste(terms, collapse=", ")))
      print(paste0(df$project_title, ", total funding $", df$award_amount))
    }
  }
  pubmed_search_term_dedup <- unique(df$pubmed_search_term)
  query_nodate <- paste(pubmed_search_term_dedup, collapse=" OR ")
  if(start_year == 0) start_year <- min(as.integer(df$fiscal_year))
  if(end_year == 1) end_year <- lubridate::year(Sys.Date())
  query <- paste0(query_nodate, " AND (", start_year, ":", end_year, "[pdat])")
  return(query)
}