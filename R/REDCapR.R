#' @title Get REDCap Report
#' @inheritParams save_project
#' @param report_id character or integer of REDCap report ID. This can be found at the end of the URL of the report.
#' @return data.frame of REDCap report
#' @export
get_REDCap_report <- function(project, report_id, silent = TRUE) {
  report_id <- as.integer(report_id)
  report <- REDCapR::redcap_report(
    redcap_uri = project$links$redcap_uri,
    token = validate_REDCap_token(project),
    report_id = report_id,
    verbose = !silent
  )
  return(report)
}
#' @noRd
get_REDCap_data <- function(project, labelled = TRUE, records = NULL, batch_size = 2000) {
  data_list <- list()
  raw <- get_REDCap_raw_data(
    project = project,
    labelled = FALSE,
    records = records,
    batch_size = batch_size
  )
  data_list <- raw %>% raw_process_redcap(project = project, labelled = labelled)
  return(data_list)
}
