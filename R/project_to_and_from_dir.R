#' @title Drop REDCap Data to Directory
#' @description
#' Exports specified records from the REDCap database (`project` object) to the specified directory.
#'
#' @details
#' This function exports the specified records from the REDCap database to the specified directory. It supports various options such as deidentification, including metadata, and merging non-repeating instruments. The function can also save the data only when it is new, and it allows for customization of file names and truncation of strings.
#'
#' @inheritParams save_project
#' @param records Character vector of records you want dropped to your directory.
#' @param deidentify Logical (TRUE/FALSE). If TRUE, deidentifies the data. Default is `FALSE`.
#' @param smart Logical (TRUE/FALSE). If TRUE, only saves when data is new. Default is `TRUE`.
#' @param include_metadata Logical (TRUE/FALSE). If TRUE, includes metadata in the export. Default is `TRUE`.
#' @param include_other Logical (TRUE/FALSE). If TRUE, includes other data in the export. Default is `TRUE`.
#' @param file_name Optional character string for adding to the front of file names.
#' @param str_trunc_length Optional integer for truncation of strings.
#' @param with_links Optional logical (TRUE/FALSE) for including links in Excel sheets. Default is `FALSE`.
#' @param separate Optional logical (TRUE/FALSE) separating each form into separate files as opposed to multi-tab Excel. Default is `FALSE`.
#' @param merge_non_repeating Optional logical (TRUE/FALSE) for merging non-repeating instruments. Default is `FALSE`.
#' @param dir_other Optional character string a different folder than project$dir_path.
#' @param forms Optional character vector for selecting specific forms to export.
#' @return Messages for confirmation.
#' @seealso
#' \link{setup_project} for initializing the `project` object.
#' @family export_functions
#' @export
drop_REDCap_to_directory <- function(
    project,
    records,
    smart = TRUE,
    deidentify = FALSE,
    include_metadata = TRUE,
    include_other = TRUE,
    with_links = TRUE,
    forms,
    merge_non_repeating = TRUE,
    separate = FALSE,
    str_trunc_length = 32000,
    file_name,
    dir_other) {
  project <- validate_project(project)
  if (deidentify) {
    project <- deidentify_project(project) # will not drop free text
  }
  if (missing(dir_other)) {
    root_dir <- get_dir(project)
    # output_dir <- file.path(root_dir, "output")
    redcap_dir <- file.path(root_dir, "REDCap", project$short_name)
  } else {
    redcap_dir <- dir_other
    bullet_in_console("Be careful setting your own directories", file = redcap_dir, bullet_type = "!")
  }
  redcap_metadata_dir <- file.path(redcap_dir, "metadata")
  redcap_other_dir <- file.path(redcap_dir, "other")
  due_for_save_metadata <- TRUE
  due_for_save_data <- TRUE
  if (smart) {
    if (!is.null(project$internals$last_metadata_dir_save)) due_for_save_metadata <- project$internals$last_metadata_update > project$internals$last_metadata_dir_save
    if (!is.null(project$internals$last_data_dir_save)) due_for_save_data <- project$internals$last_data_update > project$internals$last_data_dir_save
  }
  redcap_dir %>% dir.create(showWarnings = FALSE)
  redcap_metadata_dir %>% dir.create(showWarnings = FALSE)
  redcap_other_dir %>% dir.create(showWarnings = FALSE)
  if (due_for_save_metadata) {
    if (include_metadata) {
      project$internals$last_metadata_dir_save <- project$internals$last_metadata_update
      names_generic <- c(
        "forms",
        "fields",
        "choices",
        "arms",
        "events",
        "event_mapping",
        "missing_codes"
      )
      names_redcap <- c(
        "instruments",
        "metadata",
        "codebook",
        "arms",
        "events",
        "event_mapping",
        "missing_codes"
      )
      for (i in seq_along(names_generic)) { # ,"log" #taking too long
        z <- project$metadata[names_generic[i]]
        if (is_something(z[[1]])) {
          tn <- names_redcap[i]
          if (project$internals$use_csv) {
            list_to_csv(
              list = z,
              dir = redcap_metadata_dir,
              file_name = tn
            )
          } else {
            list_to_excel(
              list = z,
              dir = redcap_metadata_dir,
              file_name = tn,
              str_trunc_length = str_trunc_length,
              overwrite = TRUE
            )
          }
        }
      }
    }
    if (include_other) {
      for (i in seq_along(names_generic)) { # ,"log" #taking too long
        z <- project$metadata[names_generic[i]]
        if (is_something(z[[1]])) {
          tn <- names_redcap[i]
          if (project$internals$use_csv) {
            list_to_csv(
              list = z,
              dir = redcap_metadata_dir,
              file_name = tn
            )
          } else {
            list_to_excel(
              list = z,
              dir = redcap_metadata_dir,
              file_name = tn,
              str_trunc_length = str_trunc_length,
              overwrite = TRUE
            )
          }
        }
      }
      for (x in c(
        "project_info",
        # "log",
        "users"
      )) { # ,"log" #taking too long
        if (project$internals$use_csv) {
          list_to_csv(
            list = project$redcap[x],
            dir = redcap_other_dir,
            file_name = x
          )
        } else {
          list_to_excel(
            list = project$redcap[x],
            dir = redcap_other_dir,
            file_name = x,
            str_trunc_length = str_trunc_length,
            overwrite = TRUE
          )
        }
      }
    }
  }
  if (due_for_save_data) {
    project$internals$last_data_dir_save <- project$internals$last_data_update
    # if(merge_non_repeating) project <- merge_non_repeating_project(project)
    to_save_list <- project[["data"]]
    if (!missing(records)) to_save_list <- filter_project(project, filter_field = project$redcap$id_col, filter_choices = records) %>% process_df_list()
    link_col_list <- list()
    if (with_links) {
      to_save_list <- to_save_list %>% lapply(function(DF) {
        add_redcap_links_to_DF(DF, project)
      })
      link_col_list <- list(
        "redcap_link"
      )
      names(link_col_list) <- project$redcap$id_col
    }
    if (missing(file_name)) file_name <- project$short_name
    if (project$internals$use_csv) {
      list_to_csv(
        list = to_save_list,
        dir = redcap_dir,
        file_name = file_name
      )
    } else {
      list_to_excel(
        list = to_save_list,
        dir = redcap_dir,
        link_col_list = link_col_list,
        file_name = file_name,
        separate = separate,
        # header_df_list = to_save_list %>% construct_header_list(metadata = project$metadata$fields),
        # key_cols_list = construct_key_col_list(project,data_choice = "data"),
        str_trunc_length = str_trunc_length,
        overwrite = TRUE
      )
    }
    # if(merge_non_repeating) project <- unmerge_non_repeating_project(project)
  }
  return(project)
}
#' @title Reads project from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_project
#' @param allow_all logical TF for allowing project$data names that are not also form names
#' @param drop_nonredcap_vars logical TF for dropping non-redcap variable names
#' @param drop_non_form_vars logical TF for dropping non-form variable names
#' @param stop_or_warn character string of whether to stop, warn, or do nothing when forbidden cols are present
#' @return messages for confirmation
#' @export
read_from_REDCap_upload <- function(project, allow_all = TRUE, drop_nonredcap_vars = TRUE, drop_non_form_vars = TRUE, stop_or_warn = "warn") {
  project <- validate_project(project)
  root_dir <- get_dir(project)
  # output_dir <- file.path(root_dir, "output")
  redcap_dir <- file.path(root_dir, "REDCap", project$short_name)
  redcap_upload_dir <- file.path(redcap_dir, "upload")
  if (!file.exists(redcap_upload_dir)) stop("Did you forget to run `setup_project()`? No upload folder --> ", redcap_upload_dir)
  x <- list.files.real(redcap_upload_dir) %>% basename()
  if (length(x) == 0) {
    stop("No files in folder --> ", redcap_upload_dir)
  }
  df <- data.frame(
    file_name = basename(x),
    file_name_no_ext = gsub("\\.xlsx|\\.xls", "", x),
    match = NA
  )
  df$match <- strsplit(df$file_name_no_ext, "_") %>%
    lapply(function(IN) {
      IN[length(IN)]
    }) %>%
    unlist()
  df$match[which(!df$match %in% c(project$internals$merge_form_name, project$metadata$forms$form_name))] <- NA
  if (!allow_all) {
    df <- df[which(!is.na(df$match)), ]
  }
  if (project$data_update %>% is_something()) stop("Already files in project$data_update, clear that first")
  project[["data_update"]] <- list()
  for (i in seq_len(nrow(df))) { # not done yet
    the_file <- readxl::read_xlsx(file.path(redcap_upload_dir, df$file_name[i]), col_types = "text") %>% all_character_cols() # would
    drop_cols <- NULL
    if (drop_nonredcap_vars) {
      x <- colnames(the_file)[which(!colnames(the_file) %in% c(project$redcap$raw_structure_cols, project$metadata$fields$field_name))]
      drop_cols <- drop_cols %>%
        append(x) %>%
        unique()
    }
    if (drop_non_form_vars) {
      form <- df$match[i]
      if (form == project$internals$merge_form_name) form <- project$metadata$forms$form_name[which(!project$metadata$forms$repeating)]
      x <- colnames(the_file)[which(!colnames(the_file) %in% c(project$redcap$raw_structure_cols, project$metadata$fields$field_name[which(project$metadata$fields$form_name %in% form)]))]
      drop_cols <- drop_cols %>%
        append(x) %>%
        unique()
    }
    message1 <- paste0("forbidden cols name: ", df$file_name[i], "; ", x %>% paste0(collapse = ", "))
    if (length(x) > 0) {
      if (stop_or_warn == "stop") stop(message1)
      if (stop_or_warn == "warn") warning(message1, immediate. = TRUE)
    }
    the_file <- the_file[, which(!colnames(the_file) %in% drop_cols)]
    project[["data_update"]][[df$match[i]]] <- the_file
  }
  project
}
#' @noRd
default_sheet_drops <- function(project) {
  project$summary %>%
    process_df_list() %>%
    names()
}
#' @noRd
read_xl_to_project_for_upload <- function(project, file_path, drop_sheets = default_sheet_drops(project)) {
  # add data_update check
  if (!endsWith(file_path, ".xlsx")) stop("File type must be '.xlsx' --> ", file_path)
  if (!file.exists(file_path)) stop("Path does not exist --> ", file_path)
  data_list <- file_path %>%
    openxlsx::loadWorkbook() %>%
    wb_to_list()
  if (is_something(drop_sheets)) {
    message("dropping sheets from `drop_sheets` (Default is names from project$summary)... ", paste0(drop_sheets, collapse = ", "))
    for (drop_sheet in drop_sheets) {
      data_list[[drop_sheet]] <- NULL
    }
  }
  if (length(data_list) == 0) {
    message("nothing to return")
    return(project)
  }
  project$data_update <- data_list
  return(project)
}
