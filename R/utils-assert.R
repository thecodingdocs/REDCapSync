#' @noRd
assert_dir <- function(dir_path, silent = TRUE) {
  # param check
  dir_path <- clean_dir_path(dir_path)
  if (!file.exists(dir_path)) {
    stop("dir_path does not exist")
  }
  if (!is.logical(silent)) {
    stop("silent parameter must be TRUE or FALSE")
  }
  stop_mes <- "Did you use `setup_project()`?"
  for (folder in .dir_folders) {
    if (!file.exists(file.path(dir_path, folder))) {
      stop("'", dir_path, "/", folder, "' missing! ", stop_mes)
    }
  }
  if (!silent) {
    cli_alert_wrap("Directory is Valid!",
                   url = dir_path,
                   bullet_type = "v")
  }
  dir_path
}
#' @noRd
get_project_token <- function(project, silent = TRUE) {
  assert_setup_project(project)
  token_name <- project$redcap$token_name
  token <- Sys.getenv(token_name)
  valid <- is_valid_redcap_token(token = token, silent = silent)
  fake_token <- "YoUrNevErShaReToKeNfRoMREDCapWebsiTe"
  if (!silent) {
    user_renviron_path <- user_renviron()
    cli_alert_wrap(
      paste0(
        "You can set REDCap tokens each session with `Sys.setenv(",
        token_name,
        "='",
        fake_token,
        "')`... or for higher security add `",
        token_name,
        " = '",
        fake_token,
        "'` to your user .Renviron file ",
        "...(then restart R under session tab after saving ",
        "file)... The way to tell it worked is to run the code, `Sys.getenv('",
        token_name,
        "')`"
      )
    )
    cli_alert_info("User R environ file --> {.file {user_renviron_path}}")
    cli_alert_info("Try `edit_r_environ()` from `usethis` package.")
    cli_alert_info("See the {.vignette REDCapSync::Tokens} vignette.")
    if (is_something(project$links$redcap_api)) {
      cli_alert_wrap(
        paste0(
          "You can request, regenerate, or delete with ",
          "`project$url_launch('api')` or go here: "
        ),
        url = project$links$redcap_api
      )
    }
    if (valid) {
      cli_alert_wrap(
        paste0(
          "Valid token for ",
          project$project_name,
          " is set in your R session (pending connection)!"
        ),
        bullet_type = "v"
      )
    }
  }
  token
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
assert_env_name <- function(env_name,
                            arg_name = "env_name",
                            max.chars = 26L,
                            underscore_allowed_first = FALSE) {
  assert_character(arg_name,
                   len = 1L,
                   min.chars = 1L)
  assert_integerish(
    max.chars,
    len = 1L,
    lower = 1L,
    upper = 255L
  )
  assert_logical(underscore_allowed_first, len = 1L)
  assert_string(
    x = env_name,
    n.chars = NULL,
    min.chars = 1L,
    max.chars = max.chars,
    pattern = paste0(
      ifelse(underscore_allowed_first, "", "^[A-Za-z]"),
      "[A-Za-z0-9_]*$"
    ),
    fixed = NULL,
    ignore.case = TRUE,
    .var.name = arg_name
  )
  invisible(env_name)
}
#' @noRd
assert_blank_project <- function(project) {
  assert_list(
    project,
    names = "unique",
    len = length(.blank_project)
  )
  assert_names(
    names(project),
    type = "unique",
    identical.to = names(.blank_project)
  )
  invisible(project)
}
#' @noRd
assert_setup_project <- function(project) {
  assert_blank_project(project)
  assert_env_name(
    env_name = project$project_name,
    max.chars = 31L,
    arg_name = "project_name"
  )
  # DIRPATH
  assert_env_name(
    env_name = project$redcap$token_name,
    max.chars = 50L,
    arg_name = "token_name",
    underscore_allowed_first = TRUE
  )
  # dirpath
  assert_choice(
    project$internals$sync_frequency,
    choices = c("always", "hourly", "daily", "weekly", "monthly", "never")
  )
  assert_integerish(project$internals$days_of_log,
                    len = 1L,
                    lower = 1L)
  assert_logical(project$internals$get_files,
                 len = 1L)
  assert_logical(project$internals$get_file_repository,
                 len = 1L)
  assert_logical(project$internals$original_file_names,
                 len = 1L)
  assert_logical(project$internals$entire_log,
                 len = 1L)
  assert_logical(project$internals$metadata_only,
                 len = 1L)
  assert_logical(project$internals$add_default_fields,
                 len = 1L)
  assert_logical(project$internals$add_default_transformation,
                 len = 1L)
  assert_logical(project$internals$add_default_summaries,
                 len = 1L)
  assert_integerish(
    project$internals$batch_size_download,
    len = 1L,
    lower = 1L
  )
  assert_integerish(
    project$internals$batch_size_upload,
    len = 1L,
    lower = 1L
  )
  assert_choice(
    project$internals$get_type,
    choices = .get_type
  )
  # assert_web_link(project$links$redcap_base) # argName
  invisible(project)
}
#' @noRd
assert_project_details <- function(projects, nrows = NULL) {
  the_output <- assert_data_frame(
    x = projects,
    nrows = nrows,
    ncols = length(.blank_project_cols)
  )
  assert_names(colnames(projects), permutation.of = .blank_project_cols)
  the_output
}
#' @noRd
assert_project_path <- function(project_path) {
  assert_path_for_output(x = project_path,
                         overwrite = TRUE,
                         extension = "RData")
  assert_true(endsWith(basename(project_path), .project_path_suffix))
}
