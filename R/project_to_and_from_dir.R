#' @title Reads project from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_project
#' @param allow_all logical TF for allowing project$data names that are not also
#' form names
#' @param drop_nonredcap_vars logical TF for dropping non-redcap variable names
#' @param drop_non_form_vars logical TF for dropping non-form variable names
#' @param stop_or_warn character string of whether to stop, warn, or do nothing
#' when forbidden cols are present
#' @return messages for confirmation
#' @export
read_from_REDCap_upload <- function(project,
                                    allow_all = TRUE,
                                    drop_nonredcap_vars = TRUE,
                                    drop_non_form_vars = TRUE,
                                    stop_or_warn = "warn") {
  project <- assert_blank_project(project)
  root_dir <- get_dir(project)
  redcap_dir <- file.path(root_dir, "REDCap", project$short_name)
  redcap_upload_dir <- file.path(redcap_dir, "upload")
  if (!file.exists(redcap_upload_dir)) {
    stop(
      "Did you forget to run `setup_project()`? No upload folder --> ",
      redcap_upload_dir
    )
  }
  x <- list_files_real(redcap_upload_dir) %>% basename()
  if (length(x) == 0) {
    stop("No files in folder --> ", redcap_upload_dir)
  }
  df <- data.frame(
    file_name = basename(x),
    file_name_no_ext = gsub("\\.xlsx|\\.xls", "", x),
    match = NA,
    stringsAsFactors = FALSE
  )
  df$match <- strsplit(df$file_name_no_ext, "_") %>%
    lapply(function(x) {
      x[length(x)]
    }) %>%
    unlist()
  df$match[which(
    !df$match %in% c(
      project$internals$merge_form_name,
      project$metadata$forms$form_name
    )
  )] <- NA
  if (!allow_all) {
    df <- df[which(!is.na(df$match)), ]
  }
  if (project$data_updates %>% is_something()) {
    stop("Already files in project$data_updates, clear that first")
  }
  project[["data_updates"]] <- list()
  for (i in seq_len(nrow(df))) {
    # not done yet
    the_file <- readxl::read_xlsx(
      file.path(redcap_upload_dir, df$file_name[i]),
      col_types = "text"
    ) %>%
      all_character_cols() # would
    drop_cols <- NULL
    if (drop_nonredcap_vars) {
      x <- colnames(the_file)[which(
        !colnames(the_file) %in% c(
          project$redcap$raw_structure_cols,
          project$metadata$fields$field_name
        )
      )]
      drop_cols <- drop_cols %>%
        append(x) %>%
        unique()
    }
    if (drop_non_form_vars) {
      form_name <- df$match[i]
      if (form_name == project$internals$merge_form_name) {
        form_name <- project$metadata$forms$form_name[which(!project$metadata$forms$repeating)]
      }
      x <- colnames(the_file)[which(
        !colnames(the_file) %in% c(
          project$redcap$raw_structure_cols,
          project$metadata$fields$field_name[which(project$metadata$fields$form_name %in% form_name)]
        )
      )]
      drop_cols <- drop_cols %>%
        append(x) %>%
        unique()
    }
    message1 <- paste0(
      "forbidden cols name: ",
      df$file_name[i],
      "; ",
      toString(x)
    )
    if (length(x) > 0) {
      if (stop_or_warn == "stop") {
        stop(message1)
      }
      if (stop_or_warn == "warn") {
        warning(message1, immediate. = TRUE)
      }
    }
    the_file <- the_file[, which(!colnames(the_file) %in% drop_cols)]
    project[["data_updates"]][[df$match[i]]] <- the_file
  }
  project
}
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
