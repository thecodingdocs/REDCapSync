#' @noRd
assert_dir <- function(dir_path, silent = FALSE) {
  # param check
  dir_path <- clean_dir_path(dir_path)
  if (!file.exists(dir_path)) {
    stop("dir_path does not exist")
  }
  stop_mes <- "Did you use `setup_project()`?"
  for (folder in .dir_folders) {
    if (!file.exists(file.path(dir_path, folder))) {
      stop("'", dir_path, "/", folder, "' missing! ", stop_mes)
    }
  }
  cli_alert_wrap("Directory is Valid!",
                 url = dir_path,
                 bullet_type = "v",
                 silent = silent)
  dir_path
}
#' @noRd
assert_web_link <- function(link) {
  #change to validate api endpoint
  if (is.null(link))
    stop("link is NULL")
  # Check if the link starts with "https://" or "http://"
  if (!grepl("^https?://", link)) {
    stop("Invalid web link. It must start with 'http://' or 'https://'.")
  }
  # Remove trailing slash if present
  link <- gsub("/$", "", link)
  # Check if the link ends with one of the specified web endings
  if (!grepl("\\.(edu|com|org|net|gov|io|xyz|info|co|uk)$", link)) {
    stop("Invalid web link. It must end with a valid web ending",
         "(.edu, .com, etc.).")
  }
  # Add a trailing slash
  link <- paste0(link, "/")
  link
}
#' @noRd
assert_env_name <- function(x, max.chars = 26L, all_caps = FALSE) {
  assert_character(x, len = 1L, min.chars = 1L, any.missing = FALSE)
  # can change to min.length for projects
  assert_integerish(max.chars,
                    len = 1L,
                    lower = 1L,
                    upper = 255L,
                    any.missing = FALSE)
  assert_string(
    x,
    n.chars = NULL,
    min.chars = 1L,
    max.chars = max.chars,
    pattern =  "^[A-Za-z][A-Za-z0-9_]*$",
    fixed = NULL,
    ignore.case = TRUE
  )
  if (all_caps) {
    assert_string(
      x,
      n.chars = NULL,
      min.chars = 1L,
      max.chars = max.chars,
      pattern = "^[A-Z][A-Z0-9_]*$",
      fixed = NULL,
      ignore.case = FALSE
    )
  }
  invisible(x)
}
#' @noRd
test_env_name <- function(x, max.chars = 26L, all_caps = FALSE) {
  x <- try_else_null({
    suppressWarnings({
      assert_env_name(x = x,
                      max.chars = max.chars,
                      all_caps = all_caps)
    })
  })
  !is.null(x)
}
#' @noRd
test_unique_character <- function(x){
  test_character(
    x,
    min.chars = 1L,
    unique = TRUE,
    min.len = 1L,
    any.missing = FALSE
  )
}
#' @noRd
assert_blank_project <- function(project) {
  assert_list(project, names = "unique", len = length(.blank_project))
  assert_names(names(project),
               type = "unique",
               identical.to = names(.blank_project))
  invisible(project)
}
#' @noRd
assert_setup_project <- function(project) {
  assert_blank_project(project)
  assert_env_name(project$project_name, max.chars = 31L, all_caps = TRUE)
  # dir_path
  # redcap_uri
  assert_env_name(project$redcap$token_name, max.chars = 50L, all_caps = TRUE)
  assert_choice(project$internals$sync_frequency, choices = .sync_frequency)
  assert_logical(project$internals$labelled, len = 1L, any.missing = FALSE)
  assert_logical(project$internals$hard_reset, len = 1L, any.missing = FALSE)
  assert_choice(project$internals$get_type, choices = .get_type)
  assert(
    test_scalar_na(project$internals$records) ||
      test_character(
        project$internals$records, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert(
    test_scalar_na(project$internals$fields) ||
      test_character(
        project$internals$fields, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert(
    test_scalar_na(project$internals$forms) ||
      test_character(
        project$internals$forms, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert(
    test_scalar_na(project$internals$events) ||
      test_character(
        project$internals$events, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert(
    test_scalar_na(project$internals$filter_logic) ||
      test_character(
        project$internals$filter_logic, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert_logical(project$internals$metadata_only, len = 1L, any.missing = FALSE)
  assert_integerish(
    project$internals$batch_size_download,
    len = 1L,
    lower = 1L,
    any.missing = FALSE
  )
  assert_integerish(
    project$internals$batch_size_upload,
    len = 1L,
    lower = 1L,
    any.missing = FALSE
  )
  assert_logical(project$internals$entire_log,
                 len = 1L,
                 any.missing = FALSE)
  assert_integerish(
    project$internals$days_of_log,
    len = 1L,
    lower = 1L,
    any.missing = FALSE
  )
  assert_choice(project$internals$timezone, OlsonNames())
  assert_logical(project$internals$get_files,
                 len = 1L,
                 any.missing = FALSE)
  assert_logical(project$internals$get_file_repository,
                 len = 1L,
                 any.missing = FALSE)
  assert_logical(project$internals$original_file_names,
                 len = 1L,
                 any.missing = FALSE)
  assert_logical(project$internals$add_default_fields,
                 len = 1L,
                 any.missing = FALSE)
  assert_logical(
    project$internals$add_default_transformation,
    len = 1L,
    any.missing = FALSE
  )
  assert_logical(project$internals$add_default_summaries,
                 len = 1L,
                 any.missing = FALSE)
  assert_logical(project$internals$was_updated,
                 len = 1L,
                 any.missing = FALSE)
  invisible(project)
}
#' @noRd
test_setup_project <- function(project) {
  project <- tryCatch(
    expr = {
      suppressWarnings({
        assert_setup_project(project = project)
      })
    },
    error = function(e) {
      NULL
    }
  )
  !is.null(project)
}
#' @noRd
assert_project_details <- function(project_details, nrows = NULL) {
  assert_data_frame(
    x = project_details,
    nrows = nrows,
    ncols = length(.blank_project_cols)
  )
  assert_names(colnames(project_details), must.include = .blank_project_cols)
  if (nrow(project_details) > 0L) {
    project_details$project_name |>
      lapply(function(project_name) {
        assert_env_name(project_name, max.chars = 31L, all_caps = TRUE)
      })
    assert_names(project_details$project_name, type = "unique")
  }
  project_details
}
#' @noRd
test_project_details <- function(project_details, nrows = NULL) {
  project_details <- tryCatch(
    expr = {
      suppressWarnings({
        assert_project_details(project_details = project_details, nrows = nrows)
      })
    },
    error = function(e) {
      NULL
    }
  )
  !is.null(project_details)
}
#' @noRd
assert_project_path <- function(project_path) {
  assert_path_for_output(x = project_path,
                         overwrite = TRUE,
                         extension = "RData")
  assert_true(endsWith(basename(project_path), .project_path_suffix))
}
