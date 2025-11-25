#' @noRd
default_sheet_drops <- function(project) {
  # outdated! generate from summary
  project$summary %>%
    process_df_list() %>%
    names()
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
