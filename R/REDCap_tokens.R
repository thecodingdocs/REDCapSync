#' @noRd
view_project_token <- function(project) {
  project <- assert_blank_project(project)
  token <- get_project_token(project, silent = FALSE)
  if (is_something(token)) {
    cli_alert_warning(paste0("Never share your token: ", token))
  }
  invisible()
}
#' @noRd
test_project_token <- function(project) {
  assert_setup_project(project)
  rcon <- redcapConnection(url = project$links$redcap_uri,
                                      token = get_project_token(project))
  redcap_version <- tryCatch(
    expr = exportVersion(rcon = rcon),
    error = function(e) {
      NULL
    }
  )
  # add timezone
  project$internals$last_test_connection_attempt <- now_time()
  version_error <- is.null(redcap_version)
  project$internals$last_test_connection_outcome <- !version_error
  if (version_error) {
    cli_alert_danger("Your REDCap API token check failed. Check privileges.")
    return(invisible(project))
  }
  cli_alert_wrap("Connected to REDCap!",
                 url = project$links$redcap_home,
                 bullet_type = "v")
  version_changed <- FALSE
  if (!is.null(project$redcap$version)) {
    version_changed <- !identical(project$redcap$version, redcap_version)
  }
  project$redcap$version <- redcap_version
  if (project$internals$ever_connected) {
    project_info <- rcon$projectInformation()
    project_id_changed <- !identical(project$redcap$project_id,
                                     as.character(project_info$project_id))
    if (project_id_changed) {
      cli_alert_warning(
        paste0(
          "The REDCap project ID for {project$project_name} has changed",
          "since the last setup."
        )
      )
      return(invisible(project))
    }
  }
  project$internals$ever_connected <- TRUE
  if (version_changed || is.null(project$links$redcap_home)) {
    project <- update_project_links(project)
  }
  invisible(project)
}
#' @noRd
is_valid_redcap_token <- function(token, silent = TRUE) {
  start_text <- "The token "
  token_text <- NULL
  end_text <- "is not a valid 32-character hexadecimal value."
  trimmed_token <- token |> trimws(whitespace = "[\\h\\v]")
  if (is.null(token)) {
    token_text <- "is `NULL`,"
    cli_alert_danger(paste0(start_text, token_text, end_text))
    return(FALSE)
  }
  if (is.na(token)) {
    token_text <- "is `NA`,"
    cli_alert_danger(paste0(start_text, token_text, end_text))
    return(FALSE)
  }
  if (nchar(token) == 0L) {
    token_text <- "`` (empty),"
    cli_alert_danger(paste0(start_text, token_text, end_text))
    return(FALSE)
  }
  if (token != trimmed_token) {
    token_text <- "contains whitespace (extra lines) and is therefore"
    cli_alert_danger(paste0(start_text, token_text, end_text))
    return(FALSE)
  }
    if (!is_hexadecimal(token, length = 32)) {
      cli_alert_danger(paste0(start_text, token_text, end_text))
      return(FALSE)
    }
  return(TRUE)
}
#' @noRd
is_hexadecimal <- function(string, length = NULL) {
  if (!is_something(string)) {
    return(FALSE)
  }
  pattern <- if (is.null(length)) {
    "^[0-9A-Fa-f]+$" # Any length
  } else {
    paste0("^[0-9A-Fa-f]{", length, "}$") # Exact length
  }
  return(grepl(pattern, string))
}
#' @noRd
.token_prefix <- "REDCapSync_"
