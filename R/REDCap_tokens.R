#' @noRd
test_project_token <- function(project, silent = TRUE) {
  assert_setup_project(project)
  rcon <- redcapConnection(url = project$links$redcap_uri,
                           token = get_project_token(project = project,
                                                     silent = silent))
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
      cli_abort("The REDCap project ID for {project$project_name} has changed!")
      #trigger hard_rest
    } # move this?
  }
  project$internals$ever_connected <- TRUE
  if (version_changed || is.null(project$links$redcap_home)) {
    project <- update_project_links(project)
  }
  invisible(project)
}
#' @noRd
update_project_links <- function(project) {
  redcap_base <- project$links$redcap_base
  redcap_version <- project$redcap$version
  link_head <- paste0(redcap_base, "redcap_v", redcap_version)
  link_tail <- paste0("?pid=", project$redcap$project_id)
  home <- paste0("/index.php", link_tail)
  record_home <-  paste0("/DataEntry/record_home.php", link_tail)
  # record_subpage <- "/DataEntry/index.php", link_tail)
  dashboard <-  paste0("/DataEntry/record_status_dashboard.php", link_tail)
  api <-  paste0("/API/project_api.php", link_tail)
  api_playground <-  paste0("/API/playground.php", link_tail)
  setup <-  paste0("/ProjectSetup/index.php", link_tail)
  user_rights <-  paste0("/UserRights/index.php", link_tail)
  logging <-  paste0("/Logging/index.php", link_tail)
  designer <-  paste0("/Design/online_designer.php", link_tail)
  codebook <-  paste0("/Design/data_dictionary_codebook.php", link_tail)
  dictionary <-  paste0("/Design/data_dictionary_upload.php", link_tail)
  data_quality <- paste0("/DataQuality/index.php", link_tail)
  identifiers <- paste0(home, "&route=IdentifierCheckController:index")
  project$links$redcap_home <- paste0(link_head, home)
  project$links$redcap_record_home <- paste0(link_head, record_home)
  project$links$redcap_records_dashboard <- paste0(link_head, dashboard)
  project$links$redcap_api <- paste0(link_head, api)
  project$links$redcap_api_playground <- paste0(link_head, api_playground)
  project$links$redcap_setup <- paste0(link_head, setup)
  project$links$redcap_user_rights <- paste0(link_head, user_rights)
  project$links$redcap_logging <- paste0(link_head, logging)
  project$links$redcap_designer <- paste0(link_head, designer)
  project$links$redcap_codebook <- paste0(link_head, codebook)
  project$links$redcap_dictionary <- paste0(link_head, dictionary)
  project$links$redcap_data_quality <- paste0(link_head, data_quality)
  project$links$redcap_identifiers <- paste0(link_head, identifiers)
  invisible(project)
}
#' @noRd
is_valid_redcap_token <- function(token, silent = TRUE) {
  start_text <- "The token "
  token_text <- NULL
  end_text <- " is not a valid 32-character hexadecimal value."
  trimmed_token <- trimws(token, whitespace = .whitespace)
  if (is.null(token)) {
    token_text <- "is `NULL`,"
    cli_alert_wrap(
      paste0(start_text, token_text, end_text),
      bullet_type = "!",
      silent = silent
    )
    return(FALSE)
  }
  if (is.na(token)) {
    token_text <- "is `NA`,"
    cli_alert_wrap(
      paste0(start_text, token_text, end_text),
      bullet_type = "!",
      silent = silent
    )
    return(FALSE)
  }
  if (!nzchar(token)) {
    token_text <- "`` (empty),"
    cli_alert_wrap(
      paste0(start_text, token_text, end_text),
      bullet_type = "!",
      silent = silent
    )
    return(FALSE)
  }
  if (token != trimmed_token) {
    token_text <- "contains whitespace (extra lines) and is therefore"
    cli_alert_wrap(
      paste0(start_text, token_text, end_text),
      bullet_type = "!",
      silent = silent
    )
    return(FALSE)
  }
  if (!is_hexadecimal(token, length = 32L)) {
    cli_alert_wrap(
      paste0(start_text, token_text, end_text),
      bullet_type = "!",
      silent = silent
    )
    return(FALSE)
  }
  TRUE
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
  grepl(pattern, string)
}
#' @noRd
.token_prefix <- "REDCAPSYNC_"
