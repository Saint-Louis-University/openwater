#' OpenWater Get Application Programming Interface
#'
#' @param ow_api_key OpenWater API Key
#' @param ow_api_spec OpenWater API Specification URL
#' @param ow_domain OpenWater domain
#'
#' @return An object representing the OpenWater API operations and schemas
#' @export
#'
#' @examples
#' \donttest{ow_api <- ow_get_api()}
ow_get_api <- function(ow_api_key = Sys.getenv("OPENWATER_API_KEY"),
                       ow_api_spec = Sys.getenv("OPENWATER_API_SPEC"),
                       ow_domain = Sys.getenv("OPENWATER_DOMAIN")) {
  api <- rapiclient::get_api(ow_api_spec)
  headers <- c("X-ClientKey" = ow_domain, "X-ApiKey" = ow_api_key)
  ow_api <-
    list(
      operations = rapiclient::get_operations(api, headers),
      schemas = rapiclient::get_schemas(api)
    )
  class(ow_api) <- "ow_api"
  return(ow_api)
}

#' OpenWater Get Report
#'
#' @param reportId ID of the desired report
#' @param ow_api OpenWater API object
#' @param sleep The time interval to wait between checking job completion status
#' @param ... Additional arguments passed to \code{\link[readr]{read_csv}}()
#'
#' @return A \code{\link[tibble]{tibble}}
#' @export
#'
#' @examples
#' \donttest{ow_get_report(666, ow_api)}
ow_get_report <- function(reportId, ow_api, sleep = 3, ...) {
  ow_api$operations$RunReport(
    reportId = reportId,
    outputFormat = ow_api$schemas$Models.ReportRunner.RunRequest("csv")
  ) %>%
    httr::content() %>%
    { .[["jobId"]] } -> jobId
  
  repeat {
    Sys.sleep(sleep)
    ow_api$operations$GetJobById(id = jobId) %>%
      httr::content() -> response_job_details
    
    if(!(response_job_details$jobState %in% c("Enqueued", "Processing")))
      break
  }
    
  readr::read_csv(response_job_details$resultUrl, ...)
}
