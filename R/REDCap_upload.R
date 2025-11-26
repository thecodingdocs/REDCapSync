#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your
#' own risk.
#' Remember all changes are saved in the redcap log if there's an issue. Missing
#' rows and columns are fine!
#' @inheritParams save_project
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean
#' data pass your data to `labelled_to_raw_form(form,project)` first.
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
#' @keywords internal
upload_form_to_REDCap <- function(to_be_uploaded, project, batch_size = 500) {
  REDCapR::redcap_write(
    ds_to_write = to_be_uploaded %>% all_character_cols(),
    batch_size = batch_size,
    interbatch_delay = 0.2,
    continue_on_error = FALSE,
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    overwrite_with_blanks = TRUE
  )
}
#' @noRd
read_xlsx_for_upload <- function(project,
                                 summary_name,
                                 file_path) {
  if (!missing(summary_name)) {
    if (!missing(file_path)) {
      cli_alert_warning("`file_path` only needed if summary_name not provided.")
      cli_alert_info("Using `file_path` from `summary_name`...")
    }
    if (!summary_name %in% names(project$summary) %>% setdiff("all_records")) {
      stop("`summary_name` is not one of `project$summary`")
    }
    file_path <- project$summary[[summary_name]]$file_path
  }else{
    # add data_updates check
    if (!endsWith(file_path, ".xlsx")) {
      stop("File type must be '.xlsx' --> ", file_path)
    }
    if (!file.exists(file_path)) {
      stop("Path does not exist --> ", file_path)
    }
  }
  data_list <- excel_to_list(file_path)
  the_row <- which(data_list$summary_details$paramater == "raw_form_names")
  form_names <- data_list$summary_details$value[the_row] %>%
    strsplit(" [:|:] ") %>%
    unlist()
  drop_names <- names(data_list) %>% setdiff(form_names)
  # use summary_details to drop non-uploadcompatibles and warn
  for (drop_sheet in drop_names) {
    data_list[[drop_sheet]] <- NULL
  }
  if (length(data_list) == 0) {
    message("nothing to return")
    return(invisible(project))
  }
  #check data not already there
  project$data_updates <- data_list
  invisible(project)
}
