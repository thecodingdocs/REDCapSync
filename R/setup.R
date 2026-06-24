#' @rdname setup-load
#' @title Setup or Load REDCapSync Project
#' @description
#' Setup or load a REDCapSync [project] object.
#' Prepares a new REDCapSync project by recording the REDCap URI,
#' token name, sync settings, and optional data selection preferences.
#' @details
#' Unless `hard_reset = TRUE`, it will first attempt to load an
#' existing project from cache or the supplied `dir_path` before creating a
#' new project object.
#' If settings differ from the loaded project, they will be used or it will
#' trigger a hard reset with a warning. `setup_project()` itself does not
#' perform a full REDCap sync. It configures the project so that subsequent sync
#' actions may retrieve data, users, files, and logs according to the project
#' settings.
#'
#' @param project_name Character scalar. Unique uppercase project name with no
#' spaces or symbols.
#' @param dir_path Optional character scalar. Directory where REDCap project
#' files are stored. If omitted, the project is only available in the current R
#' session and cannot be persisted to disk.
#' @param redcap_uri Character scalar. Base URL of the REDCap API endpoint.
#' @param token_name Optional character scalar. Environment variable name used
#' to store the REDCap token. Defaults to `REDCAPSYNC_<project_name>`.
#' @param sync_frequency Character scalar. Allowed values are "always",
#' "hourly", "daily", "weekly", "monthly", "once", and "never". This
#' setting controls how often future syncs are considered due.
#' @param labelled Logical scalar. If `TRUE`, data will be preserved as labelled
#' values. Default is `TRUE`.
#' @param get_type Character scalar. REDCap API export type. One of
#' "identified", "deidentified", "deidentified_strict", or
#' "deidentified_super_strict". Default is "identified".
#' @param records Optional character vector of record IDs to request.
#' @param fields Optional character vector of field names to request.
#' @param forms Optional character vector of form names to request.
#' @param events Optional character vector of event names to request.
#' @param filter_logic Optional character scalar. REDCap filter logic used to
#' limit returned records or record-events.
#' @param id_position Integer scalar of the variable that uniquely identifies
#' the subject (typically record_id). This defaults to the first variable in the
#' data dictionary.
#' @param get_users Logical scalar. If `TRUE`, the project will be configured to
#' retrieve REDCap users during sync.
#' @param get_data Logical scalar. If `TRUE`, the project will be configured to
#' retrieve REDCap data during sync.
#' @param batch_size_download Integer scalar. Number of records to request per
#' download batch. Default is `1000`.
#' @param batch_size_upload Integer scalar. Number of records to process per
#' upload batch. Default is `500`.
#' @param get_entire_log Logical scalar. If `TRUE`, REDCap activity logs are
#' retrieved in full. Default is `FALSE`.
#' @param log_days Integer scalar. Number of days of log history to consider
#' when a new project is being set up or a hard reset occurs. Default is `10`.
#' @param log_drop_details Logical scalar. If `TRUE`, the log details are
#' excluded.
#' @param log_drop_exports Logical scalar. If `TRUE`, the log export events are
#' excluded.
#' @param get_files Logical scalar. If `TRUE`, file attachments are configured
#' to be retrieved from REDCap. Default is `FALSE`.
#' @param get_file_repository Logical scalar. If `TRUE`, the file repository is
#' configured to be retrieved from REDCap. Default is `FALSE`.
#' @param original_file_names Logical scalar. If `TRUE`, retrieved files keep
#' their original names. Default is `FALSE`.
#' @param add_default_datasets Logical scalar. If `TRUE`, dataset templates are
#' added to a new project. Default is `TRUE`.
#' @param timezone Optional character scalar. Time zone used for REDCap data
#' timestamps. Defaults to `Sys.timezone()`.
#' @param hard_reset Logical scalar. If `TRUE`, any existing project with the
#' same `project_name` is ignored and a fresh project object is initialized.
#' Default is `FALSE`.
#'
#' @seealso
#' \code{vignette("REDCapSync", package = "REDCapSync")}
#' \code{vignette("Tokens", package = "REDCapSync")}
#' [projects] for shortcuts of cached setup projects
#' [project] for using the project objects
#' \code{vignette("Cache", package = "REDCapSync")}
#'
#' @examples
#' # Initialize the project object with the REDCap API token and URL
#' save_folder <- tempdir() # replace with real folder
#' project <- setup_project(
#'   project_name = "FIRST_PROJECT",
#'   dir_path = save_folder,
#'   redcap_uri = "https://redcap.yourinstitution.edu/api/"
#' )
#'
#' # object can be named whatever you choose to assign
#' # TEST projects can be loaded in addition to real projects
#' project_test <- load_project("TEST_CLASSIC")
#'
#' @returns R6 project object with [REDCapSyncProject] class.
#' @export
setup_project <- function(project_name,
                          dir_path,
                          redcap_uri,
                          token_name = paste0("REDCAPSYNC_", project_name),
                          sync_frequency = "daily",
                          labelled = TRUE,
                          get_type = "identified",
                          records = NA,
                          fields = NA,
                          forms = NA,
                          events = NA,
                          filter_logic = NA,
                          id_position = 1L,
                          get_users = TRUE,
                          get_data = TRUE,
                          batch_size_download = 1000L,
                          batch_size_upload = 500L,
                          get_entire_log = FALSE,
                          log_days = 10L,
                          log_drop_details = FALSE,
                          log_drop_exports = FALSE,
                          get_files = FALSE,
                          get_file_repository = FALSE,
                          original_file_names = FALSE,
                          add_default_datasets = TRUE,
                          timezone = Sys.timezone(),
                          hard_reset = FALSE) {
  missing_dir_path <- missing(dir_path)
  if (missing_dir_path) {
    missing_dir_message <- paste("If you don't supply a directory, REDCapSync",
                                 "will only run in R session. The package is",
                                 "meant to be used with a directory.")
  }
  assert_project_name(project_name, allow_test_names = TRUE)
  assert_env_name(token_name, max.chars = 50L, all_caps = TRUE)
  assert_choice(sync_frequency, choices = SYNC_FREQUENCY)
  assert_logical(labelled, len = 1L, any.missing = FALSE)
  assert_logical(hard_reset, len = 1L, any.missing = FALSE)
  assert_choice(get_type, choices = GET_TYPE)
  assert(
    test_scalar_na(records) ||
      test_character(
        records, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert(
    test_scalar_na(fields) ||
      test_character(
        fields, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert(
    test_scalar_na(forms) ||
      test_character(
        forms, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert(
    test_scalar_na(events) ||
      test_character(
        events, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert(
    test_scalar_na(filter_logic) ||
      test_character(
        filter_logic, # add exist warning
        min.chars = 1L,
        unique = TRUE,
        min.len = 1L,
        any.missing = FALSE
      )
  )
  assert_integerish(id_position, len = 1L, lower = 1L, any.missing = FALSE)
  assert_logical(get_data, len = 1L, any.missing = FALSE)
  assert_integerish(
    batch_size_download,
    len = 1L,
    lower = 1L,
    any.missing = FALSE
  )
  assert_integerish(
    batch_size_upload,
    len = 1L,
    lower = 1L,
    any.missing = FALSE
  )
  assert_logical(get_entire_log, len = 1L, any.missing = FALSE)
  assert_integerish(log_days, len = 1L, lower = 1L, any.missing = FALSE)
  assert_choice(timezone, OlsonNames())
  assert_logical(get_files, len = 1L, any.missing = FALSE)
  assert_logical(get_file_repository, len = 1L, any.missing = FALSE)
  assert_logical(original_file_names, len = 1L, any.missing = FALSE)
  assert_logical(add_default_datasets, len = 1L, any.missing = FALSE)
  # assert redcap_uri
  original_details <- NULL
  if (!hard_reset) {
    projects <- get_projects()
    in_proj_cache <- project_name %in% projects$project_name
    was_loaded <- FALSE
    if (!config$allow.test.names()) {
      if (project_name %in% TEST_PROJECT_NAMES) {
        project <- load_test_project(project_name = project_name,
                                     dir_path = dir_path)$.internal
        was_loaded <- TRUE
      }
    }
    if (!was_loaded && in_proj_cache) {
      project <- try_else_null({
        suppressWarnings({ # will ignore supplied dir_path ? add compare
          load_project(project_name = project_name)$.internal
        })
      })
      was_loaded <- !is.null(project)
    }
    if (!was_loaded && !missing_dir_path) {
      project <- try_else_null({
        suppressWarnings({ # will ignore supplied dir_path ? add compare
          load_project_from_dir(project_name = project_name,
                                dir_path = dir_path,
                                validate = TRUE)
        })
      })
      was_loaded <- !is.null(project)
    }
    # attempt to load existing project unless hard_reset
    if (!was_loaded) {
      project <- BLANK_PROJECT
      cli_alert_warning("Setup blank project. Unable to find, load, or repair.")
    }
    if (was_loaded) {
      # compare current setting to previous settings...
      original_details <- extract_project_details(project)
      if (missing(redcap_uri)) {
        redcap_uri <- original_details$redcap_uri
      }
      if (original_details$redcap_uri != redcap_uri) {
        stop_message <- paste0("There is an existing project at your chosen",
                               " directory with same `project_name` but a",
                               " different `redcap_uri`. You can use",
                               " setup_project(..., hard_reset = TRUE) to",
                               " override.")
        cli_abort(stop_message)
      }
      if (in_proj_cache) {
        project_row <- which(projects$project_name == project_name)
        cache_details <- projects[project_row, ]
        if (original_details$project_id != cache_details$project_id) {
          stop_message <- paste0("There is an existing project at your chosen",
                                 " directory with same `project_name` but a",
                                 " different `project_id`. You can use",
                                 " setup_project(..., hard_reset = TRUE) to",
                                 " override.")
          cli_abort(stop_message)
        }
      }
      project <- reconcile_version(project)
      params <- list(
        id_position = id_position,
        labelled = labelled,
        get_type = get_type,
        records = records,
        fields = fields,
        forms = forms,
        events = events,
        filter_logic = filter_logic
      )
      for (setting_name in names(params)) {
        if (!hard_reset) {
          if (!is.null(project$settings[[setting_name]])) {
            original <- project$settings[[setting_name]]
            value <- params[[setting_name]]
            if (!identical(original, value)) {
              hard_reset <- TRUE
              cli_alert_warning(
                paste(
                  "The loaded project was `{setting_name} = {original}` and",
                  "youchose `{setting_name} = {toString(value)}`. Therefore, a",
                  "full update was triggered."
                )
              )
            }
          }
        }
      }
    }
  }
  if (hard_reset) {
    # load blank if hard_reset = TRUE
    assert_project_name(project_name)
    project <- BLANK_PROJECT
    cli_alert_info("Setup blank project because `hard_reset = TRUE`")
  }
  if (missing_dir_path) {
    if (!is_something(project$dir_path)) {
      cli_alert_warning(missing_dir_message)
    }
  }
  project$project_name <- project_name
  if (!missing_dir_path) {
    # will also ask user if provided dir is new or different
    # (will load from original but start using new dir)
    project$dir_path <- set_dir(dir_path, silent = TRUE)
    dir.create(
      path = file.path(project$dir_path, "REDCap", project_name),
      showWarnings = FALSE
    )
  }
  project$token_name <- token_name
  project$redcap$timezone <- timezone
  project$settings$sync_frequency <- sync_frequency
  project$settings$labelled <- labelled
  project$settings$get_type <- get_type
  project$settings$records <- records
  project$settings$fields <- fields
  project$settings$forms <- forms
  project$settings$events <- events
  project$settings$filter_logic <- filter_logic
  project$settings$id_position <- id_position
  project$settings$get_data <- get_data
  project$settings$batch_size_download <- batch_size_download
  project$settings$batch_size_upload <- batch_size_upload
  project$settings$get_entire_log <- get_entire_log
  project$settings$log_days <- log_days
  project$settings$get_files <- get_files
  project$settings$get_file_repository <- get_file_repository
  project$settings$original_file_names <- original_file_names
  project$settings$add_default_datasets <- add_default_datasets
  #test these
  project$links$redcap_uri <- redcap_uri # add test, should end in / or add it
  project$links$redcap_base <- redcap_uri |> dirname() |> paste0("/")
  project$internals$version <- utils::packageVersion("REDCapSync")
  project$internals$is_blank <- FALSE
  project$internals$hard_reset <- hard_reset
  project$data <- all_character_cols_list(project$data)
  if (!is_valid_redcap_token(get_project_token(project))) {
    cli_alert_warning("No valid token in session: Sys.getenv('{token_name}')")
  }
  project <- assert_setup_project(project)
  # final_details ? compare if original_details not NULL
  invisible(REDCapSyncProject$new(project))
}
#' @noRd
reconcile_version <- function(project) {
  past_version <- project$internals$version
  current_version <- utils::packageVersion("REDCapSync")
  if (!identical(past_version, current_version)) {
    project <- reset_project_datasets(project)
  }
  project$internals$version <- current_version
  project
}
#' @noRd
SYNC_FREQUENCY <- c("always",
                    "hourly",
                    "daily",
                    "weekly",
                    "monthly",
                    "once",
                    "never")
#' @noRd
GET_TYPE <- c("identified",
              "deidentified",
              "deidentified_strict",
              "deidentified_super_strict")
#' @noRd
get_project_path <- function(project_name,
                             dir_path,
                             type = "",
                             check_dir = FALSE) {
  assert_env_name(project_name, max.chars = 31L, all_caps = TRUE)
  if (check_dir) {
    assert_dir(dir_path)
  }
  assert_choice(type, PROJECT_FILE_TYPES)
  file_name <- paste0(project_name, "_REDCapSync")
  if (type != "") {
    file_name <- paste0(file_name, "_", type)
  }
  file_name <- paste0(file_name, ".RData")
  file_path <- file.path(dir_path, "R_objects", file_name)
  sanitize_path(file_path)
}
#' @noRd
get_project_path2 <- function(project,
                              type = "",
                              check_dir = FALSE) {
  assert_setup_project(project)
  project_name <- project$project_name
  dir_path <- project$dir_path
  get_project_path(
    project_name = project_name,
    dir_path = dir_path,
    type = type,
    check_dir = check_dir
  )
}
#' @noRd
PROJECT_FILE_TYPES <- c("",
                        "details",
                        "metadata",
                        "data",
                        "data_default",
                        "data_flat",
                        "data_merge_non_repeating")
#' @noRd
PROJECT_PATH_SUFFIX <- "_REDCapSync.RData"
#' @rdname setup-load
#' @export
load_project <- function(project_name) {
  assert_project_name(project_name, allow_test_names = TRUE)
  projects <- get_projects()
  has_projects <- nrow(projects) > 0L
  project_in_cache <- project_name %in% projects$project_name
  project_is_test_project <- project_name %in% TEST_PROJECT_NAMES
  if (!has_projects && !project_is_test_project) {
    end_message <- paste0("No projects in cache. Use `setup_project(...) ",
                          " and `project$sync()` or try an offline test",
                          " project. \n\nTEST projects: ",
                          toString(TEST_PROJECT_NAMES))
    cli_abort(end_message)
  }
  project <- NULL
  if (project_in_cache) {
    project_row <- which(projects$project_name == project_name)
    dir_path <- sanitize_path(projects$dir_path[project_row])
    project <- load_project_from_dir(project_name = project_name,
                                     dir_path = dir_path,
                                     validate = TRUE)
  }
  if (!project_in_cache && !project_is_test_project) {
    end_message <- paste0("No project called {project_name} in cache.",
                          " Use `setup_project(...) and `project$sync()`?",
                          " or try an offline test. \n\nYour projects: ",
                          toString(projects$project_name),
                          "; \n\nTEST projects: ",
                          toString(TEST_PROJECT_NAMES))
    cli_abort(end_message)
  }
  if (is.null(project) && project_is_test_project) {
    project <- load_test_project(project_name)$.internal
  }
  invisible(REDCapSyncProject$new(project))
}
#' @noRd
load_project_from_dir <- function(project_name, dir_path, validate = TRUE) {
  assert_env_name(project_name, max.chars = 31L, all_caps = TRUE)
  assert_dir(dir_path, silent = TRUE)
  project_path <- get_project_path(project_name = project_name,
                                   dir_path = dir_path)
  assert_project_path(project_path)
  if (!file.exists(project_path)) {
    cli_abort(
      paste0(
        "No file at path {.file {project_path}}'",
        "'. Did you use {.fn REDCapSync::setup_project} and `project$sync()`?"
      )
    )
  }
  project <- readRDS(file = project_path)
  if (!validate) {
    return(project)
  }
  project <- repair_setup_project(project)
  if (is.null(project)) {
    abort_message <- paste0("Failed to load/repair project with proper",
                            " validation. This can happen with version",
                            " changes. You can still try to load the object",
                            " with `readRDS(\"{project_path}\")`. You should",
                            " re-run `project <- setup_project(...)`",
                            " and `project$sync()`.")
    cli_abort(abort_message)
  }
  project <- assert_setup_project(project)
  #   check if in cache already and relation!
  # SAVE
  loaded_dir <- sanitize_path(project$dir_path)
  if (!identical(dir_path, loaded_dir)) {
    cli_alert_warning(
      paste0("loaded dir_path did not match your cached dir_path. This should ",
             "only happen with cloud or shared directories.")
    )
  }
  project$dir_path <- dir_path
  project_details_path <- get_project_path(project_name = project_name,
                                           dir_path = dir_path,
                                           type = "details")
  if (file.exists(project_details_path)) { # will update timestamps
    saved_project_details <- readRDS(project_details_path)
    if (saved_project_details$project_name == project$project_name) {
      project$internals$last_sync <- saved_project_details$last_sync
      project$internals$last_directory_save <-
        saved_project_details$last_directory_save
    }
  }
  project |> extract_project_details() |> add_project_details_to_cache()
  the_message <- paste0("Loaded {project$project_name}: {.path {dir_path}}")
  if (due_for_sync(project$project_name)) {
    the_message <- paste0(the_message,
                          "\n... Due for sync. Run `project$sync()` to update.")
  }
  cli_alert_success(the_message)
  project
}
#' @noRd
load_test_project <- function(project_name, dir_path = NULL) {
  if (missing(project_name)) {
    cli_alert_info(paste(
      "TEST project choices:",
      toString(TEST_PROJECT_NAMES)
    ))
    project_name <- "TEST_CLASSIC"
    cli_alert_info("Defaulting to {project_name}")
  }
  assert_choice(project_name, TEST_PROJECT_NAMES)
  .test_projects <- list(
    TEST_CLASSIC = TEST_CLASSIC,
    TEST_REPEATING = TEST_REPEATING,
    TEST_LONGITUDINAL = TEST_LONGITUDINAL,
    TEST_MULTIARM = TEST_MULTIARM,
    TEST_EDGE = TEST_EDGE,
    TEST_DATA = TEST_DATA,
    TEST_CANCER = TEST_CANCER,
    TEST_REDCAPR_SIMPLE = TEST_REDCAPR_SIMPLE,
    TEST_REDCAPR_LONGITUDINAL = TEST_REDCAPR_LONGITUDINAL,
    TEST_REDCAPR_CLIN_TRIAL = TEST_REDCAPR_CLIN_TRIAL
  )
  project <- .test_projects[[project_name]]
  project <- repair_setup_project(project)
  assert_setup_project(project)
  if (!is.null(dir_path)) {
    project$dir_path <- set_dir(dir_path)
    dir.create(
      path = file.path(project$dir_path, "REDCap", project_name),
      showWarnings = FALSE
    )
    project <- add_default_datasets(project)
  } else {
    project$dir_path <- NA
  }
  project$internals$is_test <- TRUE
  cli_alert_success("Loaded TEST project {project$project_name}!")
  cli_alert_warning("Does not actually communicate with any REDCap API")
  invisible(REDCapSyncProject$new(project))
}
#' @noRd
save_project <- function(project, silent = FALSE) {
  assert_setup_project(project)
  if (!project$internals$ever_connected) {
    cli_alert_danger(
      paste0(
        "Did not save ",
        project$project_name,
        " because there has never been a REDCap connection! You must use ",
        "`setup_project(...)` and `project$sync()`"
      )
    )
    return(invisible(project))
  }
  project$internals$last_directory_save <- now_time()
  save_path <- get_project_path2(project = project)
  saveRDS(object = project, file = save_path) # add error check
  project_details <- extract_project_details(project)
  add_project_details_to_cache(project_details)
  saveRDS(
    object = project_details,
    file = get_project_path2(project, type = "details")
  ) # add error check
  if (!silent) {
    cli_alert_success("Saved {project$project_name}! {.file {save_path}}")
  }
  invisible(project)
}
#' @noRd
TEST_REDCAPR_NAMES <- c(
  "TEST_REDCAPR_SIMPLE",
  "TEST_REDCAPR_LONGITUDINAL",
  "TEST_REDCAPR_CLIN_TRIAL"
)
#' @noRd
TEST_PROJECT_NAMES <- c(
  "TEST_CLASSIC",
  "TEST_REPEATING",
  "TEST_LONGITUDINAL",
  "TEST_MULTIARM",
  "TEST_EDGE",
  "TEST_DATA",
  "TEST_CANCER",
  TEST_REDCAPR_NAMES
)
#' @noRd
BLANK_PROJECT <- list(
  project_name = NULL,
  dir_path = NULL,
  token_name = NULL,
  redcap = list(
    project_id = NULL,
    project_title = NULL,
    version = NULL,
    project_info = NULL,
    log = NULL,
    comments = NULL,
    users = NULL,
    current_user = NULL,
    timezone = NULL,
    has_log_access = NULL,
    has_dag_access = NULL,
    has_user_access = NULL,
    has_file_repository_access = NULL
  ),
  metadata = list(
    forms = NULL,
    fields = NULL,
    choices = NULL,
    id_col = NULL,
    form_key_cols = NULL,
    arms = NULL,
    events = NULL,
    event_mapping = NULL,
    missing_codes = NULL,
    raw_structure_cols = NULL,
    is_longitudinal = NULL,
    has_arms = NULL,
    has_multiple_arms = NULL,
    has_arms_that_matter = NULL,
    has_repeating_forms_or_events = NULL,
    has_repeating_forms = NULL,
    has_repeating_events = NULL
  ),
  data = NULL,
  record_summary = NULL,
  transformation = list(
    custom = NULL,
    data = NULL,
    fields = NULL,
    field_functions = NULL,
    data_updates = NULL
  ),
  datasets = list(),
  jobs = list(),
  links = list(
    redcap_uri = NULL,
    redcap_base = NULL,
    redcap_home = NULL,
    redcap_record_home = NULL,
    redcap_record_subpage = NULL,
    redcap_records_dashboard = NULL,
    redcap_api = NULL,
    redcap_api_playground = NULL,
    redcap_setup = NULL,
    redcap_user_rights = NULL,
    redcap_logging = NULL,
    redcap_designer = NULL,
    redcap_codebook = NULL,
    redcap_dictionary = NULL,
    redcap_data_quality = NULL,
    redcap_identifiers = NULL,
    cran = "",
    help = PKGDOWN_LINK,
    github = GITHUB_LINK,
    thecodingdocs = "https://www.thecodingdocs.com/"
  ),
  settings = list(
    sync_frequency = "daily",
    labelled = TRUE,
    get_type = "identified",
    records = NA,
    fields = NA,
    forms = NA,
    events = NA,
    filter_logic = NA,
    id_position = NULL,
    get_users = TRUE,
    get_data = TRUE,
    batch_size_download = 1000L,
    batch_size_upload = 500L,
    get_entire_log = FALSE,
    log_days = 10L,
    log_drop_details = FALSE,
    log_drop_exports = FALSE,
    get_files = FALSE,
    get_file_repository = FALSE,
    original_file_names = FALSE,
    add_default_datasets = TRUE
  ),
  internals = list(
    version = NULL,
    last_test_connection_attempt = NULL,
    last_test_connection_timezone = NULL,
    last_test_connection_outcome = NULL,
    last_metadata_update = NULL,
    last_metadata_dir_save = NULL,
    last_full_update = NULL,
    last_data_update = NULL,
    last_data_dir_save = NULL,
    last_data_transformation = NULL,
    last_dataset_save = NULL,
    last_quality_check = NULL,
    last_clean = NULL,
    last_directory_save = NULL,
    last_sync = NULL,
    project_type = "redcap",
    is_blank = TRUE,
    is_test = FALSE,
    ever_connected = FALSE,
    is_clean = FALSE,
    hard_reset = FALSE,
    was_updated = FALSE
  )
)
#' @noRd
set_dir <- function(dir_path, silent = FALSE) {
  dir_path <- clean_dir_path(dir_path)
  if (!file.exists(dir_path)) {
    cli_abort("Path not found. Use absolute path or choose existing directory.")
  }
  for (folder in DIR_FOLDERS) {
    if (!file.exists(file.path(dir_path, folder))) {
      dir.create(file.path(dir_path, folder), showWarnings = FALSE)
    }
  }
  assert_dir(dir_path, silent = silent)
}
#' @noRd
DIR_FOLDERS <- c("R_objects", "output", "scripts", "input", "REDCap")
#' @noRd
clean_dir_path <- function(dir_path) {
  if (!is.character(dir_path))
    cli_abort("dir must be a character string")
  dir_path <- dir_path |>
    trimws(whitespace = WHITESPACE) |>
    sanitize_path()
  dir_path
}
#' @noRd
WHITESPACE <- "[\\h\\v]"
