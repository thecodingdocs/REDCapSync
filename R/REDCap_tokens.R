#' @title View the REDCap API Token Stored in the Session
#' @description
#' Displays the REDCap API token currently stored in the session as an
#' environment variable. It's essentially a wrapper for
#' Sys.getenv("YOUR_TOKEN_NAME"), but it also validates that the token is
#' formatted like a REDCap token and provides messgaes if not valid.
#' @details
#' This function retrieves the REDCap API token associated with the specified
#' `project` object and displays it as a message.
#' The token is not returned as an R object to maintain security.
#' Use this function to confirm the token currently in use without exposing it
#' unnecessarily.
#' @inheritParams save_project
#' @return Invisible. Prints a message displaying the stored token.
#' @family Token Functions
#' @keywords Token Functions
#' @export
view_project_token <- function(project) {
  project <- assert_blank_project(project)
  token <- get_project_token(project, silent = FALSE)
  cli_alert_warning(paste0("Never share your token: ", token))
  invisible()
}
#' @title Test REDCap API Token linked to a project Object
#' @description
#' Validates the REDCap API token stored in the `project` object by attempting a
#' connection to the REDCap server.
#' @details
#' This function tests whether the API token stored in the `project` object is
#' valid by making a request to the REDCap server.
#' If the token is invalid, the function can optionally open the REDCap login
#' page in a browser (`launch_browser`)
#' @inheritParams save_project
#' @param launch_browser Logical (TRUE/FALSE). If TRUE, launches the REDCap
#' login page in the default web browser when validation fails. Default is
#' `TRUE`.
#' @return Logical. Returns `TRUE` if the API token is valid, otherwise `FALSE`.
#' @seealso
#' \href{../articles/Tokens.html}{pkgdown article on tokens}
#' \href{https://thecodingdocs.github.io/REDCapSync/articles/Tokens.html}{pkgdown article on tokens}
#' @family Token Functions
#' @keywords Token Functions
#' @export
test_project_token <- function(project, launch_browser = TRUE) {
  assert_setup_project(project)
  rcon <- project_rcon(project)
  redcap_version <- tryCatch(
    expr = {
      redcapAPI::exportVersion(rcon)
    },
    error = function(e) {
      NULL
    }
  )
  # add timezone
  project$internals$last_test_connection_attempt <- now_time()
  version_error <- is.null(redcap_version)
  project$internals$last_test_connection_outcome <- !version_error
  if (version_error && launch_browser) {
    utils::browseURL(url = ifelse(
      is_something(project$redcap$version),
      project$links$redcap_api,
      project$links$redcap_base
    ))
    # this will fail to bring you to right URL if redcap version changes at the same time a previously valid token is no longer valid
  }
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
  if (version_changed) {
    # message here
    project <- update_project_links(project)
  }
  project$internals$ever_connected <- TRUE
  invisible(project)
}
#' @noRd
is_valid_REDCap_token <- function(token,
                                  silent = TRUE,
                                  is_a_test = FALSE) {
  start_text <- "The token "
  token_text <- NULL
  end_text <- "is not a valid 32-character hexademical value."
  trimmed_token <- token %>% trimws(whitespace = "[\\h\\v]")
  if (is_a_test) {
    allowed <- c(
      .TEST_classic_token,
      .TEST_repeating_token,
      .TEST_longitudinal_token,
      .TEST_multiarm_token
    )
    end_text <- "not a valid test token."
    if (!token %in% allowed) {
      cli_alert_danger(paste0(start_text, token_text, end_text))
      return(FALSE)
    }
  }
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
  if (!is_a_test) {
    if (!is_hexadecimal(token, length = 32)) {
      cli_alert_danger(paste0(start_text, token_text, end_text))
      return(FALSE)
    }
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
get_REDCap_token_name <- function(project) {
  token_name <- paste0(.token_prefix, assert_env_name(project$short_name))
  if (is_something(project$redcap$token_name)) {
    token_name <- project$redcap$token_name
  }
  token_name
}
#' @noRd
.token_prefix <- "REDCapSync_"
#' @noRd
.TEST_classic_token <- "FAKE32TESTTOKENCLASSIC1111111111"
#' @noRd
.TEST_repeating_token <- "FAKE32TESTTOKENREPEATING22222222"
#' @noRd
.TEST_longitudinal_token <- "FAKE32TESTTOKENLONGITUDINAL33333"
#' @noRd
.TEST_multiarm_token <- "FAKE32TESTTOKENMULTIARM444444444"
#' @noRd
get_test_token <- function(short_name) {
  em <- "`short_name` must be character of length 1 equal ..." %>%
    paste0(toString(.allowed_test_short_names))
  if (!is.character(short_name))
    stop(em)
  if (length(short_name) != 1)
    stop(em)
  if (!is_test_short_name(short_name = short_name))
    stop(em)
  token <- NA
  if (short_name == "TEST_classic") {
    token <- .TEST_classic_token
  }
  if (short_name == "TEST_repeating") {
    token <- .TEST_repeating_token
  }
  if (short_name == "TEST_longitudinal") {
    token <- .TEST_longitudinal_token
  }
  if (short_name == "TEST_multiarm") {
    token <- .TEST_multiarm_token
  }
  token
}
