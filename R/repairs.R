#' @noRd
repair_projects <- function(projects) {
  if (test_project_details(projects)) {
    return(projects)
  }
  if (!is.data.frame(projects)) {
    return(NULL)
  }
  if (nrow(projects) == 0L) {
    return(BLANK_PROJECT_DETAILS)
  }
  must_have <- c("project_name",
                 "redcap_uri",
                 "token_name",
                 "project_id",
                 "dir_path")
  if (!all(must_have %in% colnames(projects))) {
    return(NULL)
  }
  for (project_name in projects$project_name) {
    project_details <- projects[which(projects$project_name == project_name), ]
    project_details <- repair_project_details(project_details)
    if (is.null(project_details)) {
      cli_alert_danger("Cannot extract project details from '{project_name}'")
    } else {
      projects <- projects[which(projects$project_name != project_name), ] |>
        bind_rows(project_details)
    }
  }
  projects
}
#' @noRd
repair_project_details <- function(project_details) {
  project_name <- project_details$project_name
  dir_path <- project_details$dir_path
  if (is.na(dir_path)) {
    return(NULL)
  }
  project_path <- get_project_path(project_name = project_name,
                                   dir_path = dir_path)
  if (!file.exists(project_path)) {
    return(NULL)
  }
  project_details <- try_else_null({
    suppressWarnings({
      project <- readRDS(file = project_path)
      project$dir_path <- dir_path
      extract_project_details(project)
    })
  })
  project_details
}
#' @noRd
repair_setup_project <- function(project) {
  if (!is_named_list(project)) {
    return(NULL)
  }
  must_have <- c("project_name",
                 "dir_path",
                 "redcap",
                 "metadata",
                 "data",
                 "internals")
  missing_important <-
    !test_env_name(project$project_name) ||
    !test_character(project$dir_path, len = 1L) ||
    !test_character(project$links$redcap_uri,
                    len = 1L,
                    min.chars = 4L)
  if (missing_important || !all(must_have %in% names(project))) {
    return(NULL)
  }
  if (!test_setup_project(project)) {
    project$project_name <- toupper(project$project_name)
    project$token_name <- project$token_name |>
      test_env_name(max.chars = 50L, all_caps = TRUE) |>
      ifelse(project$token_name,
             paste0("REDCAPSYNC_", project$project_name))
    # redcap
    project$redcap$timezone <-
      project$redcap$timezone |>
      test_choice(OlsonNames()) |>
      ifelse(project$redcap$timezone, Sys.timezone())
    # settings
    project$settings$sync_frequency <-
      project$settings$sync_frequency |>
      test_choice(choices = SYNC_FREQUENCY) |>
      ifelse(project$settings$sync_frequency, "daily")
    project$settings$labelled <-
      project$settings$labelled |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$labelled, TRUE)
    project$settings$hard_reset <-
      project$settings$hard_reset |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$hard_reset, FALSE)
    project$settings$get_type <-
      project$settings$get_type |>
      test_choice(choices = GET_TYPE) |>
      ifelse(project$settings$get_type, "identified")
    records_na <- test_scalar_na(project$settings$records)
    records_char <- test_unique_character(project$settings$records)
    project$settings$records <- (records_na || records_char) |>
      ifelse(project$settings$records, NA)
    fields_na <- test_scalar_na(project$settings$fields)
    fields_char <- test_unique_character(project$settings$fields)
    project$settings$fields <- (fields_na || fields_char) |>
      ifelse(project$settings$fields, NA)
    forms_na <- test_scalar_na(project$settings$forms)
    forms_char <- test_unique_character(project$settings$forms)
    project$settings$forms <- (forms_na || forms_char) |>
      ifelse(project$settings$forms, NA)
    events_na <- test_scalar_na(project$settings$events)
    events_char <- test_unique_character(project$settings$events)
    project$settings$events <- (events_na || events_char) |>
      ifelse(project$settings$events, NA)
    filter_logic_na <- test_scalar_na(project$settings$filter_logic)
    filter_logic_char <- test_unique_character(project$settings$filter_logic)
    project$settings$filter_logic <- (filter_logic_na || filter_logic_char) |>
      ifelse(project$settings$filter_logic, NA)
    project$settings$id_position <-
      project$settings$id_position |>
      test_integerish(len = 1L, lower = 1L, any.missing = FALSE) |>
      ifelse(project$settings$id_position, 1L)
    project$settings$get_users <-
      project$settings$get_users |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$get_users, TRUE)
    project$settings$get_data <-
      project$settings$get_data |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$get_data, TRUE)
    project$settings$batch_size_download <-
      project$settings$batch_size_download |>
      test_integerish(len = 1L, lower = 1L, any.missing = FALSE) |>
      ifelse(project$settings$batch_size_download, 2000L)
    project$settings$batch_size_upload <-
      project$settings$batch_size_upload |>
      test_integerish(len = 1L, lower = 1L, any.missing = FALSE) |>
      ifelse(project$settings$batch_size_upload, 500L)
    project$settings$get_entire_log <-
      project$settings$get_entire_log |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$get_entire_log, FALSE)
    project$settings$log_days <-
      project$settings$log_days |>
      test_integerish(len = 1L, lower = 1L, any.missing = FALSE) |>
      ifelse(project$settings$log_days, 10L)
    project$settings$log_drop_details <-
      project$settings$log_drop_details |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$log_drop_details, FALSE)
    project$settings$log_drop_exports <-
      project$settings$log_drop_exports |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$log_drop_exports, FALSE)
    project$settings$get_files <-
      project$settings$get_files |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$get_files, FALSE)
    project$settings$get_file_repository <-
      project$settings$get_file_repository |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$get_file_repository, FALSE)
    project$settings$original_file_names <-
      project$settings$original_file_names |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$original_file_names, FALSE)
    project$settings$add_default_datasets <-
      project$settings$add_default_datasets |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$settings$add_default_datasets, TRUE)
    # internals
    project$internals$was_updated <-
      project$internals$was_updated |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$was_updated, FALSE)
  }
  project
}
