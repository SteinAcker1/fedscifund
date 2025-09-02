#' Query NSF API
#' 
#' This function is a wrapper for the NSF Awards API. It allows the user to input the API parameters 
#' into the function as regular R parameters.
#'    
#' @param printFields (default: "standard") Fields to print. If "standard", the API's default print 
#'    fields will be included. If "all", every possible print field will be included. Otherwise, 
#'    you can use a vector with the specific fields you wish to include. See link from \code{help} 
#'    for more information.
#'    
#' @param export_path (default: "") Allows you to write the output to the specified file 
#'    path.
#'    
#' @param export_fmt (default: "csv") Allows you to choose file format for \code{export_path}.
#'    Options are "csv", "excel", "rdata", "spss", and "sas". (NOTE: if using the "csv" setting, 
#'    \code{printFields} is likely to cause an error if set to "all"; use a different file type 
#'    or narrow your field selection in this case.)
#'    
#' @param r_output (default: TRUE) Allows you to choose whether or not to generate any output in 
#'    R. Keep as TRUE if you plan to use the output in this R session; switch to FALSE if you 
#'    only want to generate an export file.
#'    
#' @param help (default: FALSE) If TRUE, opens https://resources.research.gov/common/webapi/awardapisearch-v1.htm#request-parameters-notes
#'    which contains information about API fields.
#'
#' @param rpp (default: 25) Results per page; this gets passed to the API query. Set to the API
#'    maximum by default.
#' 
#' @param ... Additional parameters to be included in the API query. You can find a list at the 
#'.   webpage referenced when using the \code{help} parameter. Use either a string or a vector.
#'.   If vector, elements will be bound with "+", corresponding to an "OR" operator. If string, 
#'    it will be searched in double quotations. Note that vectors CANNOT contain multiple 
#'    multi-word terms; if you want to search both "University of Iowa" and "Iowa State University",
#'    for instance, this will need to be two different queries.
#' 
#' @return A data frame containing information matching the criteria provided.
#' 
#' @example examples/query_nsf_api_examples.R
#' 
#' @export
query_nsf_api <- function(
    printFields = "standard",
    export_path = "",
    export_fmt = "csv",
    r_output = TRUE,
    help = FALSE,
    rpp = 25,
    ...
) {
  options(warn = 1)
  if(help) {
    browseURL("https://resources.research.gov/common/webapi/awardapisearch-v1.htm#request-parameters-notes")
    return()
  }
  if(identical(printFields, "all")) {
    if(identical(export_fmt, "csv") & !identical(export_path, "")) {
      warning("You are trying to write all print fields to a CSV file. This is likely to result in errors or other unexpected behavior. Consider writing to Excel instead.")
    }
    printFields <- c(
      "rpp",
      "offset",
      "id",
      "agency",
      "awardeeCity",
      "awardeeCountryCode",
      "awardeeDistrictCode",
      "awardeeName",
      "awardeeStateCode",
      "awardeeZipCode",
      "awdSpAttnCode",
      "awdSpAttnDesc",
      "cfdaNumber",
      "coPDPI",
      "date",
      "startDate",
      "expDate",
      "estimatedTotalAmt",
      "fundsObligatedAmt",
      "ueiNumber",
      "fundProgramName",
      "parentUeiNumber",
      "pdPIName",
      "perfCity",
      "perfCountryCode",
      "perfDistrictCode",
      "perfLocation",
      "perfStateCode",
      "perfZipCode",
      "poName",
      "primaryProgram",
      "transType",
      "title",
      "awardee",
      "poPhone",
      "poEmail",
      "awardeeAddress",
      "perfAddress",
      "publicationResearch",
      "publicationConference",
      "fundAgencyCode",
      "awardAgencyCode",
      "projectOutComesReport",
      "abstractText",
      "piFirstName",
      "piMiddeInitial",
      "piLastName",
      "piEmail"
    )
  }
  if(identical(printFields, "standard")) criteria <- list(rpp = rpp, ...)
  else criteria <- list(rpp = rpp, printFields = printFields, ...)
  query <- "http://api.nsf.gov/services/v1/awards.json?"
  names <- names(criteria)
  params <- c()
  for(i in names) {
    val <- as.character(criteria[[i]])
    if(i == "printFields") val <- paste0(val, collapse=",")
    else {
      if(length(val) > 1) val <- paste0(val, collapse="+")
   #   print(val)
      if(grepl(" ", val)) {
        val <- paste("%22", val, "%22", sep="")
        val <- stringr::str_replace_all(val, " ", "+")
      }
    }
    val <- paste0(c(i, "=", val), collapse="")
    params <- c(params, val)
  }
  params <- paste0(params, collapse="&")
  page_results <- rpp
  page_number <- 0
  while(page_results == rpp) {
    offset <- (page_number * rpp)
    params_offset <- paste0(params, "&offset=", offset)
    query_offset <- paste0(query, params_offset)
    new_res <- httr::GET(query_offset)
    new_res <- rawToChar(new_res$content)
    new_res <- jsonlite::fromJSON(new_res)
    new_res <- new_res$response$award
    page_results <- nrow(new_res)
    if(is.null(page_results)) page_results <- 0
    else {
      if(page_number == 0) res <- new_res
      else res <- dplyr::bind_rows(res, new_res)
      Sys.sleep(1)
      print(paste("Results", (25 * page_number) + 1, "to", (25 * page_number) + page_results, "fetched; now letting the API sleep for 1 second"))
      page_number <- page_number + 1
    }
  }
  res <- data.frame(res)
  print("Done fetching!")
  if(export_path != "") {
    print(paste0("Writing results to ", export_path, "; if you get an error, consider manually selecting fields"))
    if(export_fmt == "csv") try(write.csv(res, file = export_path))
    else if(export_fmt == "excel") try(writexl::write_xlsx(res, path = export_path))
    else if(export_fmt == "rdata") try(save(res, file = export_path))
    else if(export_fmt == "spss") try(haven::write_sav(res, path = export_path))
    else if(export_fmt == "sas") try(haven::write_xpt(res, path = export_path))
  }
  if(r_output) return(res)
}
