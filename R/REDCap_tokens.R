#' @title Set a REDCap API Token to Your Current R Session
#' @description
#' Prompts the user to input a valid REDCap API token and stores it as an
#' environment variable for the current R session.
#' Instead of using this function you should consider setting your token within
#' your REnviron file which can be edited with \code{\link[usethis]{edit_r_environ}}.
#' @details
#' If a valid token already exists in the R session, the function notifies the
#' user and asks whether they want to replace it.
#' The user is guided to provide a new token through the console.
#' It is strongly discouraged to include API tokens directly within R scripts.
#' The token is validated internally and stored using `Sys.setenv()`.
#' @inheritParams save_project
#' @param ask Logical (TRUE/FALSE). If TRUE, asks the user for confirmation before overwriting an existing valid token. Default is `TRUE`.
#' @return Invisible. A message is printed to confirm the token is successfully set.
#' @seealso
#' For the function to work you need to have a valid project object from \code{\link{setup_project}()}.
#' See our \href{https://thecodingdocs.github.io/REDCapSync/articles/Tokens.html}{REDCap Tokens Article}
#' @family Token Functions
#' @keywords Token Functions
#' @export
set_REDCap_token <- function(project, ask = TRUE) {
  project <- assert_project(project)
  token_name <- get_REDCap_token_name(project)
  is_a_test <- is_test_project(project)
  answer <- 1
  if (ask) {
    token <- assert_REDCap_token(project)
    if (is_valid_REDCap_token(token)) {
      bullet_in_console(paste0("You already have a valid token in your R session (pending test connection) '", token, "'."))
      answer <- utils::menu(choices = c("Yes", "No"), title = "Are you sure you want to set something else?")
    }
  }
  if (answer == 1) {
    has_valid_REDCap_token <- FALSE
    if (is_something(project$links$redcap_API)) {
      if (!ask) {
        bullet_in_console(paste0("You can request/regenerate/delete with `link_API_token(project)` or go here: "), url = project$links$redcap_API)
      }
    }
    if (is_a_test) {
      bullet_in_console(paste0("This is only a test so the token is: ", get_test_token(project$short_name)), bullet_type = ">")
    }
    prompt <- paste0("What is your ", project$short_name, " REDCap API token: ")
    while (!has_valid_REDCap_token) {
      token <- readline(prompt)
      has_valid_REDCap_token <- is_valid_REDCap_token(token, silent = FALSE, is_a_test = is_a_test)
    }
    do.call(Sys.setenv, stats::setNames(list(token), token_name))
  }
  assert_REDCap_token(project, silent = FALSE)
  return(invisible())
}
#' @title View the REDCap API Token Stored in the Session
#' @description
#' Displays the REDCap API token currently stored in the session as an environment variable. It's essentially a wrapper for Sys.getenv("YOUR_TOKEN_NAME"), but it also validates that the token is formatted like a REDCap token and provides messgaes if not valid.
#' @details
#' This function retrieves the REDCap API token associated with the specified `project` object and displays it as a message.
#' The token is not returned as an R object to maintain security.
#' Use this function to confirm the token currently in use without exposing it unnecessarily.
#' @inheritParams save_project
#' @return Invisible. Prints a message displaying the stored token.
#' @family Token Functions
#' @keywords Token Functions
#' @export
view_REDCap_token <- function(project) {
  project <- assert_project(project)
  token <- assert_REDCap_token(project, silent = FALSE)
  bullet_in_console(paste0("Never share your token: ", token), bullet_type = "!")
  return(invisible())
}
#' @title Test REDCap API Token linked to a project Object
#' @description
#' Validates the REDCap API token stored in the `project` object by attempting a connection to the REDCap server.
#' @details
#' This function tests whether the API token stored in the `project` object is valid by making a request to the REDCap server.
#' If the token is invalid, the function can optionally open the REDCap login page in a browser (`launch_browser`) and/or reset the token (`set_if_fails`) using the console.
#' @inheritParams save_project
#' @param set_if_fails Logical (TRUE/FALSE). If TRUE and test connection fails, asks user to paster token into consult using `set_REDCap_token(project)` function. Default is `TRUE`.
#' @param launch_browser Logical (TRUE/FALSE). If TRUE, launches the REDCap login page in the default web browser when validation fails. Default is `TRUE`.
#' @return Logical. Returns `TRUE` if the API token is valid, otherwise `FALSE`.
#' @seealso
#' \href{../articles/Tokens.html}{pkgdown article on tokens}
#' \href{https://thecodingdocs.github.io/REDCapSync/articles/Tokens.html}{pkgdown article on tokens}
#' @family Token Functions
#' @keywords Token Functions
#' @export
test_REDCap_token <- function(project, set_if_fails = TRUE, launch_browser = TRUE) {
  # token <- assert_REDCap_token(project, silent = FALSE)
  version <- get_REDCap_version(project, show_method_help = FALSE) %>%
    suppressWarnings()
  project$internals$last_test_connection_attempt <- Sys.time()
  ERROR <- is.na(version)
  project$internals$last_test_connection_outcome <- !ERROR
  if (!set_if_fails) {
    return(project)
  }
  if (ERROR && launch_browser) {
    utils::browseURL(url = ifelse(is_something(project$redcap$version), project$links$redcap_API, project$links$redcap_base))
    # this will fail to bring you to right URL if redcap version changes at the same time a previously valid token is no longer valid
  }
  while (ERROR) {
    bullet_in_console("Your REDCap API token check failed. Invalid token or API privileges. Contact Admin!`", bullet_type = "x")
    if (set_if_fails) {
      set_REDCap_token(project, ask = FALSE)
      version <- get_REDCap_version(project, show_method_help = FALSE) %>%
        suppressWarnings()
      ERROR <- is.na(version)
      project$internals$last_test_connection_outcome <- !ERROR
    }
  }
  bullet_in_console("Connected to REDCap!", bullet_type = "v")
  project$redcap$version <- version
  project$internals$ever_connected <- TRUE
  return(project)
}
#' @noRd
is_valid_REDCap_token <- function(token, silent = TRUE, is_a_test = FALSE) {
  start_text <- "The token "
  token_text <- NULL
  end_text <- "is not a valid 32-character hexademical value."
  trimmed_token <- token %>% trimws(whitespace = "[\\h\\v]")
  if (is_a_test) {
    allowed <- c(
      internal_TEST_classic_token,
      internal_TEST_repeating_token,
      internal_TEST_longitudinal_token,
      internal_TEST_multiarm_token
    )
    end_text <- "not a valid test token."
    if (!token %in% allowed) {
      bullet_in_console(
        text = paste0(start_text, token_text, end_text),
        bullet_type = "x", silent = silent
      )
      return(FALSE)
    }
  }
  if (is.null(token)) {
    token_text <- "is `NULL`,"
    bullet_in_console(
      text = paste0(start_text, token_text, end_text),
      bullet_type = "x", silent = silent
    )
    return(FALSE)
  }
  if (is.na(token)) {
    token_text <- "is `NA`,"
    bullet_in_console(
      text = paste0(start_text, token_text, end_text),
      bullet_type = "x", silent = silent
    )
    return(FALSE)
  }
  if (nchar(token) == 0L) {
    token_text <- "`` (empty),"
    bullet_in_console(
      text = paste0(start_text, token_text, end_text),
      bullet_type = "x", silent = silent
    )
    return(FALSE)
  }
  if (token != trimmed_token) {
    token_text <- "contains whitespace (extra lines) and is therefore"
    bullet_in_console(
      text = paste0(start_text, token_text, end_text),
      bullet_type = "x", silent = silent
    )
    return(FALSE)
  }
  if (!is_a_test) {
    if (!is_hexadecimal(token, length = 32)) {
      bullet_in_console(
        text = paste0(start_text, token_text, end_text),
        bullet_type = "x", silent = silent
      )
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
  token_name <- paste0(
    internal_REDCapSync_token_prefix,
    assert_env_name(project$short_name)
  )
  if (is_something(project$redcap$token_name)) {
    token_name <- project$redcap$token_name
  }
  return(token_name)
}
#' @noRd
check_saved_REDCapSync_tokens <- function() {
  the_names <- Sys.getenv() %>% names()
  the_names <- the_names[
    which(startsWith(the_names, internal_REDCapSync_token_prefix))
  ]
  if (length(the_names) == 0) {
    bullet_in_console(
      "No known REDCap tokens saved in session...",
      bullet_type = "x"
    )
    return(invisible())
  }
  the_names <- gsub(internal_REDCapSync_token_prefix, "", the_names)
  ltn <- length(the_names)
  bullet_in_console(
    paste0(
      "There are ", ltn,
      " known REDCap tokens saved in the session: ",
      as_comma_string(the_names)
    ),
    bullet_type = "x"
  )
  return(invisible())
}
#' @noRd
internal_REDCapSync_token_prefix <- "REDCapSync_"
#' @noRd
internal_TEST_classic_token <- "FAKE32TESTTOKENCLASSIC1111111111"
#' @noRd
internal_TEST_repeating_token <- "FAKE32TESTTOKENREPEATING22222222"
#' @noRd
internal_TEST_longitudinal_token <- "FAKE32TESTTOKENLONGITUDINAL33333"
#' @noRd
internal_TEST_multiarm_token <- "FAKE32TESTTOKENMULTIARM444444444"
#' @noRd
get_test_token <- function(short_name) {
  em <- "`short_name` must be character of length 1 equal ..." %>%
    paste0(as_comma_string(internal_allowed_test_short_names))
  if (!is.character(short_name)) stop(em)
  if (length(short_name) != 1) stop(em)
  if (!is_test_short_name(short_name = short_name)) stop(em)
  token <- NA
  if (short_name == "TEST_classic") {
    token <- internal_TEST_classic_token
  }
  if (short_name == "TEST_repeating") {
    token <- internal_TEST_repeating_token
  }
  if (short_name == "TEST_longitudinal") {
    token <- internal_TEST_longitudinal_token
  }
  if (short_name == "TEST_multiarm") {
    token <- internal_TEST_multiarm_token
  }
  return(token)
}
