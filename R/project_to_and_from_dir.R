#' @noRd
default_sheet_drops <- function(project) {
  #outdated! generate from summary
  project$summary %>%
    process_df_list() %>%
    names()
}
#' @noRd
read_xl_to_project_for_upload <- function(project,
                                          summary_name,
                                          file_path,
                                          drop_sheets = default_sheet_drops(project)) {
  # add data_updates check
  if (!endsWith(file_path, ".xlsx")) {
    stop("File type must be '.xlsx' --> ", file_path)
  }
  if (!file.exists(file_path)) {
    stop("Path does not exist --> ", file_path)
  }
  if (!missing(summary_name)) {
    if (!missing(file_path)) {
      cli_alert_warning("`file_path` only needed if summary_name not provided.")
      cli_alert_info("Using `file_path` from `summary_name`...")
    }
    if (!summary_name %in% names(project$summary)) {
      stop("`summary_name` is not one of `project$summary`")
    }
    file_path <- project$summary[[summary_name]]$file_path
  }
  form_list <- file_path %>%
    openxlsx::loadWorkbook() %>%
    wb_to_list()
  if (is_something(drop_sheets)) {
    message(
      "dropping sheets from `drop_sheets` ... ",
      toString(drop_sheets)
    )
    for (drop_sheet in drop_sheets) {
      form_list[[drop_sheet]] <- NULL
    }
  }
  if (length(form_list) == 0) {
    message("nothing to return")
    return(invisible(project))
  }
  project$data_updates <- form_list
  invisible(project)
}
