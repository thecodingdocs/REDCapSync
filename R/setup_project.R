#' @rdname setup-load
#' @title Setup or Load REDCapSync Project
#' @description
#' Setup or Load the `project` object for pipeline.
#'
#' @details
#' This function sets up the `project` object by storing the REDCap API token
#' and
#' other configurations required for interacting with the REDCap server.
#' It ensures that the token is valid and ready for use in subsequent API calls.
#' Neither function directly attempts communication with REDCap.
#'
#' `setup_project` is used the first time you initialize/link a REDCap project.
#' Mainly, it sets your unique `project_name` and your intended directory.
#' Unless you run \code{hard_reset = TRUE} the default will first try
#' load_project. dir_path is technically optional but without it the user cannot
#' save/load/update projects. Must be all capital letters!
#'
#' `load_project` can be used with just the `project_name` parameter after you
#' have
#' already run `setup_project` in the past with an established directory.
#' `dir_path`
#' is optional for this function but can be used if you relocated the directory.
#'
#' @param project_name A character string with no spaces or symbols representing
#' the unique short name for the REDCap project.
#' @param dir_path Optional character string representing the directory path
#' where you want the REDCap project data to be stored. If missing, project
#' object
#' will only be in current R session.
#' @param redcap_uri A character string representing the base URL of the REDCap
#' server.
#' @param token_name An optional character string for setting your token name.
#' Default is `REDCAPSYNC_<project_name>`
#' @param sync_frequency Frequency of sync. Options are "always", "hourly",
#' 'daily', 'weekly', "monthly",and "never". The check is only triggered by
#' calling the function, but can be automated with other packages.
#' Default is `daily`
#' @param labelled Logical. If `TRUE`, the data will be converted to labelled.
#' Default is `TRUE`.
#' @param hard_reset Logical (TRUE/FALSE). If TRUE, forces the setup even if the
#' `project` object already exists. Default is `FALSE`.
#' @param days_of_log Integer. Number of days to be checked in the log if a
#' hard_reset
#' or new project is setup. Default is `10`.
#' @param timezone optional timezone set of the REDCap server. Otherwise, will
#' assume Sys.timezone. Options from `OlsonNames()`.
#' @param get_files Logical (TRUE/FALSE). If TRUE, retrieves files from REDCap.
#' Default is `FALSE`.
#' @param get_file_repository Logical (TRUE/FALSE). If TRUE, retrieves file
#' repository
#' from REDCap.
#' Default is `FALSE`.
#' @param original_file_names Logical (TRUE/FALSE). If TRUE, uses original file
#' names for retrieved files. Default is `FALSE`.
#' @param entire_log Logical (TRUE/FALSE). If TRUE, retrieves the entire log.
#' Default is `FALSE`.
#' @param metadata_only Logical (TRUE/FALSE). If TRUE, updates only the
#' metadata. Default is `FALSE`.
#' @param add_default_fields Logical (TRUE/FALSE). If TRUE,
#' will add default fields
#' @param add_default_transformation Logical (TRUE/FALSE). If TRUE,
#' will add default transformation
#' @param add_default_summaries Logical (TRUE/FALSE). If TRUE,
#' will add default summaries
#' @param get_type optional character of REDCap API call type.
#' data as if user ran `sync_project`. Default is `FALSE`.
#' @param batch_size_download Integer. Number of records to process in each
#' batch. Default is `2000`.
#' @param batch_size_upload Integer. Number of records to process in each batch.
#' Default is `500`.
#' @inheritParams REDCapR::redcap_read
#' @return REDCapSync `project` list object.
#' @seealso
#' \code{\link[REDCapSync]{get_projects}} for retrieving a list of projects from
#' the directory cache.
#' @examplesIf FALSE
#' # Initialize the project object with the REDCap API token and URL
#' project <- setup_project(
#'   project_name = "TEST",
#'   dir_path = "path/to/secure/file/storage",
#'   redcap_uri = "https://redcap.yourinstitution.edu/api/"
#' )
#' project <- load_project("TEST")
#' @family project object
#' @export
setup_project <- function(project_name,
                          dir_path,
                          redcap_uri,
                          token_name = paste0("REDCAPSYNC_", project_name),
                          sync_frequency = "daily",
                          labelled = TRUE,
                          hard_reset = FALSE,
                          get_type = "identified",
                          records = NA,
                          fields = NA,
                          forms = NA,
                          events = NA,
                          filter_logic = NA,
                          metadata_only = FALSE,
                          batch_size_download = 2000L,
                          batch_size_upload = 500L,
                          entire_log = FALSE,
                          days_of_log = 10L,
                          timezone = Sys.timezone(),
                          get_files = FALSE,
                          get_file_repository = FALSE,
                          original_file_names = FALSE,
                          add_default_fields = FALSE,
                          add_default_transformation = FALSE,
                          add_default_summaries = TRUE) {
  #add better message for all caps!
  assert_env_name(project_name, max.chars = 31L, all_caps = TRUE)
  # dir_path
  # redcap_uri
  assert_env_name(token_name, max.chars = 50L, all_caps = TRUE)
  assert_choice(sync_frequency, choices = .sync_frequency)
  assert_logical(labelled, len = 1L, any.missing = FALSE)
  assert_logical(hard_reset, len = 1L, any.missing = FALSE)
  assert_choice(get_type, choices = .get_type)
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
  assert_logical(metadata_only, len = 1L, any.missing = FALSE)
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
  assert_logical(entire_log, len = 1L, any.missing = FALSE)
  assert_integerish(days_of_log, len = 1L, lower = 1L, any.missing = FALSE)
  assert_choice(timezone, OlsonNames())
  assert_logical(get_files, len = 1L, any.missing = FALSE)
  assert_logical(get_file_repository, len = 1L, any.missing = FALSE)
  assert_logical(original_file_names, len = 1L, any.missing = FALSE)
  assert_logical(add_default_fields, len = 1L, any.missing = FALSE)
  assert_logical(add_default_transformation, len = 1L, any.missing = FALSE)
  assert_logical(add_default_summaries, len = 1L, any.missing = FALSE)
  if (missing(redcap_uri)) {
    #use options or Renviron?
  }
  missing_dir_path <- missing(dir_path)
  original_details <- NULL
  if (!hard_reset) {
    projects <- get_projects()
    in_proj_cache <- project_name %in% projects$project_name
    if (!in_proj_cache && !missing_dir_path) {
      #this will add existing project to cache if not there already
    }
    was_loaded <- FALSE
    # attempt to load existing project unless hard_reset
    project <- tryCatch(
      expr = {
        suppressWarnings({
          load_project(project_name = project_name)$.internal
        })
      },
      error = function(e) {
        NULL
      }
    )
    was_loaded <- !is.null(project)
    if (!was_loaded) {
      project <- .blank_project
      cli_alert_warning("Setup blank project. Nothing in cache or directory.")
    }
    if (was_loaded) {
      # compare current setting to previous settings...
      original_details <- extract_project_details(project)
      projects <- get_projects()
      cache_details <- projects[which(projects$project_name == project_name), ]
      if (original_details$redcap_uri != redcap_uri) {
        stop("There is an existing project at your chosen directory with same ",
             "`project_name` but a different `redcap_uri`. You can use ",
             "setup_project(..., hard_reset = TRUE) to override."
        )
      }
      if (original_details$project_id != cache_details$project_id) {
        stop("There is an existing project at your chosen directory with same ",
             "`project_name` but a different `redcap_uri`. You can use ",
             "setup_project(..., hard_reset = TRUE) to override."
        )
      }
      if (!is.null(project$internals$labelled)) {
        if (project$internals$labelled != labelled) {
          load_type <- ifelse(project$internals$labelled, "labelled", "raw")
          chosen_type <- ifelse(labelled, "labelled", "raw")
          hard_reset <- TRUE
          warning(
            "The project that was loaded was ",
            load_type,
            " and you chose ",
            chosen_type,
            ". Therefore, a full update was triggered to avoid data conflicts",
            immediate. = TRUE
          )
        }
      }
    }
  }
  if (hard_reset) {
    # load blank if hard_reset = TRUE
    project <- .blank_project
    cli_alert_wrap("Setup blank project because `hard_reset = TRUE`")
  }
  if (missing_dir_path) {
    if (!is_something(project$dir_path)) {
      cli_alert_warning(paste0(
        "If you don't supply a directory, REDCapSync will only run ",
        "in R session. Package is meant to be used with a directory."))
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
  project$links$redcap_uri <- redcap_uri # add test, should end in / or add it
  project$redcap$token_name <- token_name
  project$internals$sync_frequency <- sync_frequency
  project$internals$labelled <- labelled
  project$internals$hard_reset <- hard_reset
  project$internals$get_type <- get_type
  project$internals$records <- records
  project$internals$fields <- fields
  project$internals$forms <- forms
  project$internals$events <- events
  project$internals$filter_logic <- filter_logic
  project$internals$metadata_only <- metadata_only
  project$internals$batch_size_download <- batch_size_download
  project$internals$batch_size_upload <- batch_size_upload
  project$internals$entire_log <- entire_log
  project$internals$days_of_log <- days_of_log
  project$internals$timezone <- timezone
  project$internals$get_files <- get_files
  project$internals$get_file_repository <- get_file_repository
  project$internals$original_file_names <- original_file_names
  project$internals$add_default_fields <- add_default_fields
  project$internals$add_default_transformation <- add_default_transformation
  project$internals$add_default_summaries <- add_default_summaries
  #test theese
  project$links$redcap_base <- redcap_uri |> dirname() |> paste0("/")
  project$internals$is_blank <- FALSE
  project$data <- all_character_cols_list(project$data)
  if (!is_valid_redcap_token(get_project_token(project))) {
    cli_alert_warning(
      paste0("No valid token in session: Sys.getenv('", token_name, "')"))
  }
  project <- assert_setup_project(project)
  if (!is.null(original_details)) {
    # final_details <- project |> extract_project_details()
    # message about changes compared to original
  }
  invisible(REDCapSync_project$new(project))
}
.sync_frequency <- c("always",
                     "hourly",
                     "daily",
                     "weekly",
                     "monthly",
                     "once",
                     "never")
#' @noRd
.get_type <- c("identified",
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
  assert_choice(type, .project_file_types)
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
.project_file_types <- c("", "transformation", "details")
#' @noRd
.project_path_suffix <- "_REDCapSync.RData"
#' @rdname setup-load
#' @export
load_project <- function(project_name) {
  # add load by path option
  projects <- get_projects()
  if (nrow(projects) == 0L) {
    stop("No projects in cache")
  }
  if (!project_name %in% projects$project_name) {
    stop(
      "No project called ",
      project_name,
      ". Did you use `setup_project(...)` and `project$sync()`? Options: ",
      toString(projects$project_name)
    )
  }
  dir_path <- projects$dir_path[
    which(projects$project_name == project_name)] |> sanitize_path()
  assert_dir(dir_path)
  if (!file.exists(dir_path)) {
    stop("`dir_path` doesn't exist: '", dir_path, "'")
  }
  project_path <-
    get_project_path(project_name = project_name, dir_path = dir_path)
  assert_project_path(project_path)
  if (!file.exists(project_path)) {
    stop(
      "No file at path '",
      project_path,
      "'. Did you use `setup_project()` and `project$sync()`?"
    )
  }
  project <- readRDS(file = project_path)
  project <- repair_setup_project(project)
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
  if (file.exists(project_details_path)) {
    saved_project_details <- readRDS(project_details_path)
    if (saved_project_details$project_name == project$project_name) {
      project$internals$last_sync <- saved_project_details$last_sync
      project$internals$last_directory_save <-
        saved_project_details$last_directory_save
    }
  }
  project |> extract_project_details() |> add_project_details_to_cache()
  the_message <- paste0("Loaded {project$project_name}!")
  if (due_for_sync(project$project_name)) {
    the_message <- paste0(
      the_message, " Due for sync. Run `project$sync()` to update.")
  }
  cli_alert_success(the_message)
  invisible(REDCapSync_project$new(project))
}
#' @rdname setup-load
#' @export
load_test_project <- function(project_name) {
  if (missing(project_name)) {
    cli_alert_info(paste(
      "TEST project choices:",
      toString(.test_project_names)
    ))
    project_name <- "TEST_CLASSIC"
    cli_alert_info("Defaulting to {project_name}")
  }
  assert_choice(project_name, .test_project_names)
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
  project$internals$is_test <- TRUE
  project <- REDCapSync_project$new(project)
  cli_alert_success("Loaded TEST project {project$project_name}!")
  cli_alert_warning("Does not actually communicate with any REDCap API")
  invisible(project)
}
#' @noRd
save_project <- function(project, silent = FALSE) {
  assert_setup_project(project)
  # assert_setup_project(project)
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
  save_project_path <- get_project_path2(project = project)
  saveRDS(object = project, file = save_project_path) # add error check
  project_details <- extract_project_details(project)
  add_project_details_to_cache(project_details)
  saveRDS(
    object = project_details,
    file = get_project_path2(project, type = "details")
    ) # add error check
  cli_alert_wrap(
    paste0("Saved ", project$project_name, "!"),
    url = save_project_path,
    bullet_type = "v",
    silent = silent
  )
  invisible(project)
}
#' @noRd
.test_redcapr_names <- c(
  "TEST_REDCAPR_SIMPLE",
  "TEST_REDCAPR_LONGITUDINAL",
  "TEST_REDCAPR_CLIN_TRIAL"
)
#' @noRd
.test_project_names <- c(
  "TEST_CLASSIC",
  "TEST_REPEATING",
  "TEST_LONGITUDINAL",
  "TEST_MULTIARM",
  "TEST_EDGE",
  "TEST_DATA",
  "TEST_CANCER",
  .test_redcapr_names
)
#' @noRd
.blank_project <- list(
  project_name = NULL,
  dir_path = NULL,
  redcap = list(
    token_name = NULL,
    project_id = NULL,
    project_title = NULL,
    version = NULL,
    project_info = NULL,
    log = NULL,
    users = NULL,
    current_user = NULL
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
  data_updates = NULL,
  quality_checks = NULL,
  transformation = list(
    forms = NULL,
    fields = NULL,
    field_functions = NULL,
    data_updates = NULL
  ),
  summary = list(),
  internals = list(
    last_test_connection_attempt = NULL,
    last_test_connection_timezone = NULL,
    last_test_connection_outcome = NULL,
    last_metadata_update = NULL,
    last_metadata_dir_save = NULL,
    last_full_update = NULL,
    last_data_update = NULL,
    last_data_dir_save = NULL,
    last_data_transformation = NULL,
    last_summary = NULL,
    last_quality_check = NULL,
    last_clean = NULL,
    last_directory_save = NULL,
    last_sync = NULL,
    labelled = NULL,
    timezone = NULL,
    get_files = NULL,
    get_file_repository = NULL,
    original_file_names = NULL,
    days_of_log = NULL,
    entire_log = NULL,
    project_type = "redcap",
    is_blank = TRUE,
    is_test = FALSE,
    ever_connected = FALSE,
    is_clean = FALSE,
    hard_reset = FALSE,
    was_updated = FALSE
  ),
  links = list(
    redcap_uri = NULL,
    redcap_base = NULL,
    redcap_home = NULL,
    redcap_record_home = NULL,
    # redcap_record_subpage = NULL,
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
    pkgdown = "https://thecodingdocs.github.io/REDCapSync/",
    github = "https://github.com/thecodingdocs/REDCapSync/",
    thecodingdocs = "https://www.thecodingdocs.com/"
  )
)
#' @noRd
set_dir <- function(dir_path, silent = FALSE) {
  dir_path <- clean_dir_path(dir_path)
  if (!file.exists(dir_path)) {
    stop("Path not found. Use absolute path or choose an existing directory.")
  }
  for (folder in .dir_folders) {
    if (!file.exists(file.path(dir_path, folder))) {
      dir.create(file.path(dir_path, folder), showWarnings = FALSE)
    }
  }
  assert_dir(dir_path, silent = silent)
}
#' @noRd
.dir_folders <- c("R_objects", "output", "scripts", "input", "REDCap")
#' @noRd
clean_dir_path <- function(dir_path) {
  if (!is.character(dir_path))
    stop("dir must be a character string")
  dir_path <- dir_path |>
    trimws(whitespace = .whitespace) |>
    sanitize_path()
  dir_path
}
.whitespace <- "[\\h\\v]"
