#' @noRd
get_project_token <- function(project, silent = TRUE) {
  assert_setup_project(project)
  project_name <- project$project_name
  token_source <- "envvar"
  token <- get_project_envvar_token(project)
  if (is.null(token)) {
    token_source <- "keyring"
    token <- get_project_keyring_token(project)
  }
  valid <- is_valid_redcap_token(token = token, silent = silent)
  if (!silent) {
    token_help_message(project)
    if (valid) {
      cli_alert_success("Valid token for {project_name} from {token_source}!")
      cli_alert_info("REDCap Home {.url {project$links$redcap_home}}")
    }
  }
  invisible(token)
}
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
  if (is.null(token)) { # obsolete
    token_text <- "is `NULL`,"
    cli_alert_wrap(
      paste0(start_text, token_text, end_text),
      bullet_type = "!",
      silent = silent
    )
    return(FALSE)
  }
  if (is.na(token)) { # obsolete
    token_text <- "is `NA`,"
    cli_alert_wrap(
      paste0(start_text, token_text, end_text),
      bullet_type = "!",
      silent = silent
    )
    return(FALSE)
  }
  if (!nzchar(token)) { # obsolete
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
get_project_keyring_token <- function(project) {
  if (!is_valid_keyring()) {
    return(NULL)
  }
  token <- try_else_null({
    key_get(service = config$keyring.service(),
            username = project$project_name,
            keyring = config$keyring())
  })
  token
}
#' @noRd
get_project_envvar_token <- function(project) {
  token_name <- project$redcap$token_name
  token <- Sys.getenv(token_name)
  if (!is_something(token)) {
    return(NULL)
  }
  token
}
#' @noRd
set_project_keyring_token <- function(project) {
  if (!is_valid_keyring()) {
    return(NULL)
  }
  project_name <- project$project_name
  key_set(service = config$keyring.service(),
          username = project$project_name,
          keyring = config$keyring(),
          prompt = paste0(project_name, " REDCap API token: "))
}
#' @noRd
get_project_envvar_token <- function(project) {
  token_name <- project$redcap$token_name
  token <- Sys.getenv(token_name)
  if (!is_something(token)) {
    return(NULL)
  }
  token
}
#' @noRd
has_keyring_token <- function(project_names = NULL) {
  projects <- get_projects()
  if (nrow(projects) == 0L) {
    return(NULL)
  }
  if (is.null(project_names)) {
    project_names <- projects$project_name
  }
  project_rows <- which(projects$project_name %in% project_names)
  if (length(project_rows) == 0L) {
    return(NULL)
  }
  projects <- projects[project_rows, ]
  if (!is_valid_keyring()) {
    return(NULL)
  }
  keys <- key_list(service = config$keyring.service(),
                   keyring = config$keyring())
  if (nrow(keys) == 0L) {
    return(NULL)
  }
  project_names %in% keys$username
}
#' @noRd
is_valid_keyring <- function (service = config$keyring.service(),
                              keyring = config$keyring()) {
  if (!has_keyring_pkg()) {
    cli_alert_info("`keyring` package can be used to store tokens...")
    cli_code("install.packages(\"keyring\")")
    return(FALSE)
  }
  if (!is.null(keyring)) {
    if (!has_keyring_support()) {
      cli_alert_info("Your system does not support keyring. Set to NULL.")
      cli_code("keyring::has_keyring_support()")
      return(FALSE)
    }
    # consider what will happen in ubuntu
    if (keyring_is_locked(keyring = keyring)) {
      warning_message <- "keyring = NULL (the system keyring) is locked"
      keyring_code_start <- paste0("keyring::keyring_unlock(")
      keyring_code_end <- ") # enter system password"
      if (!is.null(keyring)) {
        warning_message <- "keyring = \"{keyring}\" is locked"
        keyring_code_end <- paste0("keyring = \"", keyring, "\")")
      }
      cli_alert_warning(warning_message)
      cli_alert_info("it can be unlocked with following code ...")
      cli_code(paste0(keyring_code_start, keyring_code_end))
      return(FALSE)
    }
    if (!keyring %in% keyring_list()$keyring) {
      cli_alert_warning("keyring = \"{keyring}\" does not exist!")
      cli_alert_info("it can be created with following code ...")
      cli_code(paste0("keyring::keyring_create(\"", keyring, "\")"))
      return(FALSE)
    }
  }
  TRUE
}
#' @noRd
key_get <- function (service,
                     username = NULL,
                     keyring = NULL) {
  keyring::key_get(service = service,
                   username = username,
                   keyring = keyring)
}
#' @noRd
key_set <- function (service,
                     username = NULL,
                     keyring = NULL,
                     prompt = "Password: ") {
  keyring::key_set(service = service,
                   username = username,
                   keyring = keyring,
                   prompt = prompt)
}
#' @noRd
key_list <- function (service,
                      keyring = NULL) {
  keyring::key_list(service = service,
                    keyring = keyring)
}
#' @noRd
has_keyring_pkg <- function() {
  requireNamespace("keyring", quietly = TRUE)
}
#' @noRd
has_keyring_support <- function() {
  keyring::has_keyring_support()
}
#' @noRd
keyring_is_locked <- function(keyring = NULL){
  keyring::keyring_is_locked(keyring = keyring)
}
#' @noRd
keyring_list <- function(){
  keyring::keyring_list()
}
#' @noRd
has_envvar_token <- function(project_names = NULL) {
  projects <- get_projects()
  if (nrow(projects) == 0L) {
    return(NULL)
  }
  if (is.null(project_names)) {
    project_names <- projects$project_name
  }
  projects <- projects[which(projects$project_name %in% project_names), ]
  if (nrow(projects) == 0L) {
    return(NULL)
  }
  if (!is_valid_keyring()) {
    return(NULL)
  }
  t_names <- projects$token_name[match(project_names, projects$project_name)]
  t_names %in% names(Sys.getenv())
}
#' @noRd
token_check <- function() {
  projects <- get_projects()
  if (nrow(projects) == 0L) {
    return(NULL)
  }
  projects$has_envvar_token <- FALSE
  ennvar_check <- has_envvar_token(projects$project_name)
  if (!is.null(ennvar_check)) {
    projects$has_envvar_token <- ennvar_check
  }
  projects$has_keyring_token <- FALSE
  keyring_check <- has_keyring_token(projects$project_name)
  if (!is.null(keyring_check)) {
    projects$has_keyring_token <- keyring_check
  }
  projects
}
#' @noRd
token_help_message <- function(project) {
  assert_setup_project(project)
  user_renviron_path <- Sys.getenv("R_ENVIRON_USER", unset = NA_character_)
  if (is.na(user_renviron_path) || !nzchar(user_renviron_path)) {
    user_renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
  }
  user_renviron_path <- normalizePath(path = user_renviron_path,
                                      winslash = "/",
                                      mustWork = FALSE)
  cli_alert_info("You can set REDCap tokens the following ways ...")
  cli_alert_info("See the {.vignette REDCapSync::Tokens} vignette.")
  cli_alert_info("1. environment variables (preferred)")
  cli_li("User R environ file --> {.file {user_renviron_path}}") # windows?
  cli_li("Try `edit_r_environ()` from `usethis` package.")
  cli_li(paste0("Add `{project$redcap$token_name} = '",
                .fake_token,
                "'` to your user .Renviron file... then save the file ",
                "and restart R under session tab ... The way to tell it worked",
                " is to run `Sys.getenv('{project$redcap$token_name}')`"))
  cli_alert_info("2. keyring") # add more text
  cli_alert_info("3. console") # add more text
  cli_alert_info("x. scripts (not recommended)")
  if (is_something(project$links$redcap_api)) {
    cli_alert_wrap(
      paste0(
        "You can request, regenerate, or delete with ",
        "`project$url_launch('api')` or click link: "
      ),
      url = project$links$redcap_api
    )
  }
}
#' @noRd
.token_prefix <- "REDCAPSYNC_"
#' @noRd
.fake_token <- "YoUrNevErShaReToKeNfRoMREDCapWebsiTe"
