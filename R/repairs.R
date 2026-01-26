#' @noRd
repair_projects <- function(projects) {
  if (test_project_details(projects)) {
    return(projects)
  }
  if (!is.data.frame(projects)) {
    return(NULL)
  }
  if (nrow(projects) == 0L) {
    return(.blank_project_details)
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
  project_details <- tryCatch(
    expr = {
      suppressWarnings({
        project <- readRDS(file = project_path)
        project$dir_path <- dir_path
        extract_project_details(project)
      })
    },
    error = function(e) {
      NULL
    }
  )
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
    project$redcap$token_name <- project$redcap$token_name |>
      test_env_name(max.chars = 50L) |>
      ifelse(project$redcap$token_name,
             paste0("REDCapSync_", project$project_name))
    project$internals$sync_frequency <-
      project$internals$sync_frequency |>
      test_choice(choices = .sync_frequency) |>
      ifelse(project$internals$sync_frequency, "daily")
    project$internals$labelled <-
      project$internals$labelled |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$labelled, TRUE)
    project$internals$hard_reset <-
      project$internals$hard_reset |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$hard_reset, FALSE)
    project$internals$get_type <-
      project$internals$get_type |>
      test_choice(choices = .get_type) |>
      ifelse(project$internals$get_type, "identified")
    project$internals$records <-
      (test_scalar_na(project$internals$records) ||
          test_character(
            project$internals$records,
            # add exist warning
            min.chars = 1L,
            unique = TRUE,
            min.len = 1L,
            any.missing = FALSE
          )
      ) |>
      ifelse(project$internals$records, NA)
    project$internals$fields <-
      (test_scalar_na(project$internals$fields) ||
          test_character(
            project$internals$fields,
            # add exist warning
            min.chars = 1L,
            unique = TRUE,
            min.len = 1L,
            any.missing = FALSE
          )
      ) |>
      ifelse(project$internals$fields, NA)
    project$internals$forms <-
      (test_scalar_na(project$internals$forms) ||
          test_character(
            project$internals$forms,
            # add exist warning
            min.chars = 1L,
            unique = TRUE,
            min.len = 1L,
            any.missing = FALSE
          )
      ) |>
      ifelse(project$internals$forms, NA)
    project$internals$events <-
      (test_scalar_na(project$internals$events) ||
          test_character(
            project$internals$events,
            # add exist warning
            min.chars = 1L,
            unique = TRUE,
            min.len = 1L,
            any.missing = FALSE
          )
      ) |>
      ifelse(project$internals$events, NA)
    project$internals$filter_logic <-
      (test_scalar_na(project$internals$filter_logic) ||
          test_character(
            project$internals$filter_logic,
            # add exist warning
            min.chars = 1L,
            unique = TRUE,
            min.len = 1L,
            any.missing = FALSE
          )
      ) |>
      ifelse(project$internals$filter_logic, NA)
    project$internals$metadata_only <-
      project$internals$metadata_only |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$metadata_only, FALSE)
    project$internals$batch_size_download <-
      project$internals$batch_size_download |>
      test_integerish(len = 1L, lower = 1L, any.missing = FALSE) |>
      ifelse(project$internals$batch_size_download, 2000L)
    project$internals$batch_size_upload <-
      project$internals$batch_size_upload |>
      test_integerish(len = 1L, lower = 1L, any.missing = FALSE) |>
      ifelse(project$internals$batch_size_upload, 500L)
    project$internals$entire_log <-
      project$internals$entire_log |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$entire_log, FALSE)
    project$internals$days_of_log <-
      project$internals$days_of_log |>
      test_integerish(len = 1L, lower = 1L, any.missing = FALSE) |>
      ifelse(project$internals$days_of_log, 10L)
    project$internals$timezone <-
      project$internals$timezone |>
      test_choice(OlsonNames()) |>
      ifelse(project$internals$timezone, Sys.timezone())
    project$internals$get_files <-
      project$internals$get_files |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$get_files, FALSE)
    project$internals$get_file_repository <-
      project$internals$get_file_repository |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$get_file_repository, FALSE)
    project$internals$original_file_names <-
      project$internals$original_file_names |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$original_file_names, FALSE)
    project$internals$add_default_fields <-
      project$internals$add_default_fields |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$add_default_fields, FALSE)
    project$internals$add_default_transformation <-
      project$internals$add_default_transformation |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$add_default_transformation, FALSE)
    project$internals$add_default_summaries <-
      project$internals$add_default_summaries |>
      test_logical(len = 1L, any.missing = FALSE) |>
      ifelse(project$internals$add_default_summaries, TRUE)
  }
  project
}
