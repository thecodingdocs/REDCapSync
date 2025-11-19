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
#' Mainly, it sets your unique `short_name` and your intended directory.
#' Unless you run \code{hard_reset = TRUE} the default will first try load_project.
#' dir_path is technically optional but without it the user cannot
#' save/load/update projects.
#'
#' `load_project` can be used with just the `short_name` parameter after you
#' have
#' already run `setup_project` in the past with an established directory.
#' `dir_path`
#' is optional for this function but can be used if you relocated the directory.
#'
#' @param short_name A character string with no spaces or symbols representing
#' the unique short name for the REDCap project.
#' @param dir_path Optional character string representing the directory path
#' where you want the REDCap project data to be stored. If missing, project
#' object
#' will only be in current R session.
#' @param redcap_uri A character string representing the base URL of the REDCap
#' server.
#' @param token_name An optional character string for setting your token name.
#' Default is `REDCapSync_<short_name>`
#' @param sync_frequency Frequency of sync. Options are "always", "hourly",
#' 'daily', 'weekly', "monthly",and "never". The check is only triggered by
#' calling the function, but can be automated with other packages.
#' Default is `daily`
#' @param labelled Logical. If `TRUE`, the data will be converted to labelled.
#' Default is `TRUE`.
#' @param hard_reset Logical (TRUE/FALSE). If TRUE, forces the setup even if the
#' `project` object already exists. Default is `FALSE`.
#' @param use_csv Logical (TRUE/FALSE). If TRUE, uses CSV files for data
#' storage. Default is `FALSE`.
#' @param days_of_log Integer. Number of days to be checked in the log if a
#' hard_reset
#' or new project is setup. Default is `10`.
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
#' @param with_data Logical (TRUE/FALSE). If TRUE, loads the test project
#' object with data included.
#' @param get_type optional character of REDCap API call type.
#' data as if user ran `sync_project`. Default is `FALSE`.
#' @param batch_size_download Integer. Number of records to process in each
#' batch. Default is `2000`.
#' @param batch_size_upload Integer. Number of records to process in each batch.
#' Default is `500`.
#' @param silent Logical (TRUE/FALSE). For messages.
#' @inheritParams REDCapR::redcap_read
#' @return REDCapSync `project` list object.
#' @seealso
#' \code{\link[REDCapSync]{get_projects}} for retrieving a list of projects from
#' the directory cache.
#' @examplesIf FALSE
#' # Initialize the project object with the REDCap API token and URL
#' project <- setup_project(
#'   short_name = "TEST",
#'   dir_path = "path/to/secure/file/storage",
#'   redcap_uri = "https://redcap.yourinstitution.edu/api/"
#' )
#' project <- load_project("TEST")
#' @family project object
#' @export
setup_project <- function(short_name,
                          dir_path,
                          redcap_uri,
                          token_name = paste0("REDCapSync_", short_name),
                          sync_frequency = "daily",
                          labelled = TRUE,
                          hard_reset = FALSE,
                          records = NULL,
                          fields = NULL,
                          forms = NULL,
                          events = NULL,
                          filter_logic = NULL,
                          get_type = "identified",
                          metadata_only = FALSE,
                          batch_size_download = 2000,
                          batch_size_upload = 500,
                          entire_log = FALSE,
                          days_of_log = 10,
                          get_files = FALSE,
                          get_file_repository = FALSE,
                          original_file_names = FALSE,
                          add_default_fields = FALSE,
                          add_default_transformation = FALSE,
                          add_default_summaries = TRUE,
                          use_csv = FALSE,
                          silent = FALSE) {
  collected <- makeAssertCollection()
  assert_env_name(
    env_name = short_name,
    max.chars = 31,
    arg_name = "short_name",
    add = collected
  )
  # DIRPATH
  # need checks on setup project if project id is same
  assert_env_name(
    env_name = token_name,
    max.chars = 50,
    arg_name = "token_name",
    underscore_allowed_first = TRUE,
    add = collected
  )
  assert_logical(hard_reset, len = 1, add = collected)
  assert_logical(labelled, len = 1, add = collected)
  assert_integerish(days_of_log,
                    len = 1,
                    lower = 1,
                    add = collected)
  assert_logical(get_files, len = 1, add = collected)
  assert_logical(get_file_repository, len = 1, add = collected)
  assert_logical(original_file_names, len = 1, add = collected)
  assert_logical(entire_log, len = 1, add = collected)
  assert_logical(metadata_only, len = 1, add = collected)
  assert_logical(add_default_fields, len = 1, add = collected)
  assert_logical(add_default_transformation, len = 1, add = collected)
  assert_logical(add_default_summaries, len = 1, add = collected)
  # assert_env_name(
  #   merge_form_name,
  #   max.chars = 31,
  #   arg_name = "merge_form_name",
  #   add = collected
  # )
  assert_logical(use_csv, len = 1, add = collected)
  assert_choice(
    get_type,
    choices = .get_type,
    add = collected
  )
  assert_choice(
    sync_frequency,
    choices = c("always", "hourly", "daily", "weekly", "monthly", "never"),
    add = collected
  )
  assert_integerish(batch_size_download,
                    len = 1,
                    lower = 1,
                    add = collected)
  assert_integerish(batch_size_upload,
                    len = 1,
                    lower = 1,
                    add = collected)
  assert_logical(silent, len = 1, add = collected)
  if (missing(redcap_uri)) {
    REDCapSync_REDCAP_URI <- Sys.getenv("REDCapSync_REDCAP_URI")
    if (is_something(REDCapSync_REDCAP_URI)) {
      redcap_uri <- REDCapSync_REDCAP_URI
    }
  }
  current_function <- as.character(current_call())[[1]]
  if (!collected$isEmpty()) {
    message <- collected %>% cli_message_maker(function_name = current_function)
    cli::cli_abort(message)
  }
  projects <- get_projects() # add short_name conflict check id-base url differs
  short_name <- assert_env_name(short_name)
  sweep_dirs_for_cache(project_names = short_name)
  if (paste0(.token_prefix, short_name) != token_name) {
  } # maybe a message
  token_name <- assert_env_name(token_name)
  in_proj_cache <- short_name %in% projects$short_name
  missing_dir_path <- missing(dir_path)
  is_a_test <- is_test_short_name(short_name = short_name)
  was_loaded <- FALSE
  original_details <- NULL
  if (!in_proj_cache && !missing_dir_path) {
    project_details_path <- get_project_path(short_name = short_name,
                                             dir_path = dir_path,
                                             type = "details")
    if (file.exists(project_details_path)) {
      project_details <- tryCatch(
        expr = {
          suppressWarnings({
            project_details <- readRDS(file = project_details_path)
            project_details <- assert_project_details(project_details)
          })
        },
        error = function(e) {
          NULL
        }
      )
      if (!is.null(project_details)) {
        add_project_details_to_cache(project_details)
      } else {
        cli_alert_warning("currupted project_details so will be overwritten")
      }
      # add check for if it was loaded from right place
    }
  }
  if (!hard_reset) {
    project <- tryCatch(
      expr = {
        suppressWarnings({
          load_project(short_name = short_name)
        })
      },
      error = function(e) {
        NULL
      }
    )
    was_loaded <- !is.null(project)
    if (!was_loaded) {
      if (is_a_test) {
        project <- load_test_project(short_name = short_name, with_data = FALSE)
      } else {
        project <- .blank_project
      }
      cli_alert_wrap(
        "Setup blank project object because nothing found in cache or directory.",
        bullet_type = "!",
        silent = silent
      )
    }
    if (was_loaded) {
      # compare current setting to previous settings...
      original_details <- project %>% extract_project_details()
      if (!is.null(project$internals$labelled)) {
        if (project$internals$labelled != labelled) {
          if (!hard_reset) {
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
  }
  if (hard_reset) {
    # load blank if hard_reset = TRUE
    project <- .blank_project
    cli_alert_wrap(paste0("Setup blank project object because `hard_reset = TRUE`"),
                   silent = silent)
  }
  if (missing_dir_path) {
    # if missing the directory path from setup or load then let user know nothing will be stored
    if (!is_something(project$dir_path)) {
      # only show message if load_project wasn't used internally (that has a directory)
      cli::cli_alert_warning(
        "If you don't supply a directory, REDCapSync will only run in R session. Package is best with a directory."
      )
    }
  }
  if (!missing_dir_path) {
    project$dir_path <- set_dir(dir_path) # will also ask user if provided dir is new or different (will load from original but start using new dir)
    dir.create(
      path = file.path(project$dir_path, "REDCap", short_name),
      showWarnings = FALSE
    )
  }
  project$short_name <- short_name
  project$redcap$token_name <- token_name
  project$internals$sync_frequency <- sync_frequency
  project$internals$labelled <- labelled
  project$internals$use_csv <- use_csv
  project$internals$original_file_names <- original_file_names
  project$internals$entire_log <- entire_log
  project$internals$days_of_log <- days_of_log %>%
    assert_integerish() %>%
    as.integer()
  project$internals$get_files <- get_files
  project$internals$get_type <- get_type
  project$internals$metadata_only <- metadata_only
  project$internals$add_default_fields <-
    add_default_fields
  project$internals$add_default_transformation <-
    add_default_transformation
  project$internals$add_default_summaries <-
    add_default_summaries
  project$internals$batch_size_download <- batch_size_download %>%
    assert_integerish() %>%
    as.integer()
  project$internals$batch_size_upload <- batch_size_upload %>%
    assert_integerish() %>%
    as.integer()
  project$internals$get_file_repository <- get_file_repository
  if (is_a_test) {
    cli_alert_wrap(
      "Test objects ignore the `redcap_uri` argument and will not communicate with the REDCap API.",
      silent = silent
    )
  } else {
    project$links$redcap_uri <- redcap_uri # add test function, should end in / or add it
    project$links$redcap_base <- redcap_uri %>%
      dirname() %>%
      paste0("/") # add test function
  }
  project$internals$use_csv <- use_csv
  project$internals$is_blank <- FALSE
  project$data <- project$data %>% all_character_cols_list()
  if (!is_valid_REDCap_token(get_project_token(project), is_a_test = is_a_test)) {
    cli::cli_alert_warning(paste0("No valid token in session: Sys.getenv('", token_name, "')"))
  }
  project <- assert_setup_project(project, silent = silent)
  if (!is.null(original_details)) {
    # final_details <- project %>% extract_project_details()
    # message about changes compared to original
  }
  project$internals$last_directory_save <- now_time()
  save_project_details(project)
  invisible(project)
}
.get_type <- c("identified",
               "deidentified",
               "deidentified_strict",
               "deidentified_super_strict")
get_project_path <- function(short_name,
                             dir_path,
                             type = "",
                             check_dir = FALSE) {
  assert_env_name(short_name)
  if (check_dir) {
    assert_dir(dir_path)
  }
  checkmate::assert_choice(type, .project_file_types)
  file_name <- paste0(short_name, "_REDCapSync")
  if (type != "")
    file_name <- file_name %>% paste0("_", type)
  file_name <- file_name %>% paste0(".RData")
  file_path <- file.path(dir_path, "R_objects", file_name)
  sanitize_path(file_path)
}
get_project_path2 <- function(project,
                              type = "",
                              check_dir = FALSE) {
  assert_setup_project(project)
  short_name <- project$short_name
  dir_path <- project$dir_path
  get_project_path(
    short_name = short_name,
    dir_path = dir_path,
    type = type,
    check_dir = check_dir
  )
}
.project_file_types <- c("", "transformation", "details")
.project_path_suffix <- "_REDCapSync.RData"
#' @rdname setup-load
#' @export
load_project <- function(short_name) {
  # add load by path option
  projects <- get_projects()
  if (nrow(projects) == 0) {
    stop("No projects in cache")
  }
  if (!short_name %in% projects$short_name) {
    stop(
      "No project named ",
      short_name,
      " in cache. Did you use `setup_project()` and `sync_project()`?"
    )
  }
  dir_path <- projects$dir_path[which(projects$short_name == short_name)] %>%
    sanitize_path()
  assert_dir(dir_path)
  if (!file.exists(dir_path)) {
    stop("`dir_path` doesn't exist: '", dir_path, "'")
  }
  project_path <- get_project_path(short_name = short_name, dir_path = dir_path)
  assert_project_path(project_path)
  if (!file.exists(project_path)) {
    stop(
      "No file at path '",
      project_path,
      "'. Did you use `setup_project()` and `sync_project()`?"
    )
  }
  project <- readRDS(file = project_path)
  project <- project %>% assert_setup_project(silent = FALSE)
  #   check if in cache already and relation!
  # SAVE
  loaded_dir <- project$dir_path %>% sanitize_path()
  if (!identical(dir_path, loaded_dir)) {
    cli_alert_warning(
      "loaded dir_path did not match your cached dir_path. This should only happen with cloud/shared directories."
    )
  }
  project$dir_path <- dir_path
  project_details_path <- get_project_path2(project, type = "details")
  project <- add_project_details_to_project(project = project,
                                            project_details = readRDS(file = project_details_path))
  project$dir_path <- dir_path # why twice?
  save_project_details(project)
  invisible(project)
}
compare_project_details <- function(from, to) {
  assert_project_details(from)
  assert_project_details(to)
  collected <- makeAssertCollection()
  assert_set_equal(from$short_name, to$short_name, add = collected)
  assert_set_equal(from$project_id, to$project_id, add = collected)
  assert_set_equal(from$redcap_uri, to$redcap_uri, add = collected)
}
#' @rdname setup-load
#' @export
load_test_project <- function(short_name = "TEST_repeating",
                              with_data = FALSE) {
  em <- "`short_name` must be character string of length 1 equal to one of the following: " %>% paste0(toString(.allowed_test_short_names))
  if (!is.character(short_name))
    stop(em)
  if (length(short_name) != 1)
    stop(em)
  if (!is_test_short_name(short_name = short_name))
    stop(em)
  project <- .blank_project
  project$short_name <- short_name
  project$internals$is_test <- TRUE
  if (with_data) {
    if (short_name == "TEST_classic") {
    }
    if (short_name == "TEST_repeating") {
    }
    if (short_name == "TEST_longitudinal") {
    }
    if (short_name == "TEST_multiarm") {
    }
  }
  invisible(project)
}
#' @noRd
is_test_short_name <- function(short_name) {
  short_name %in% .allowed_test_short_names
}
#' @noRd
is_test_project <- function(project) {
  (project$short_name %in% .allowed_test_short_names) &&
    project$internals$is_test
}
#' @rdname save-deleteproject
#' @title Save or Delete project file from the directory
#' @param project A validated `project` object containing REDCap project data
#' and
#' settings. Generated using \link{load_project} or \link{setup_project}
#' @inheritParams setup_project
#' @description
#' This will save/delete the "<short_name>_REDCapSync.RData" file in the given
#' project
#' directories R_objects folder. These are optional functions given that
#' `save_project` is a also handled by a default parameter in `sync_project.`
#'
#' @details delete_project will not delete any other files from that directory.
#' The
#' user must delete any other files manually.
#' @return Message
#' @family project object
#' @export
save_project <- function(project, silent = FALSE) {
  assert_setup_project(project)
  # assert_setup_project(project)
  if (!project$internals$ever_connected) {
    cli::cli_alert_danger(
      paste0(
        "Did not save ",
        project$short_name,
        " because there has never been a REDCap connection! You must use `setup_project()` and `sync_project()`"
      )
    )
    return(invisible(project))
  }
  project$internals$last_directory_save <- now_time()
  save_project_path <- get_project_path2(project = project)
  saveRDS(object = project, file = save_project_path) # add error check
  save_project_details(project)
  cli_alert_wrap(
    paste0("Saved ", project$short_name, "!"),
    url = save_project_path,
    bullet_type = "v",
    silent = silent
  )
  invisible(project)
}
#' @title nav_to_dir
#' @inheritParams save_project
#' @return opens browser link
#' @export
nav_to_dir <- function(project) {
  utils::browseURL(project$dir_path)
}
#' @noRd
.allowed_test_short_names <- c("TEST_classic",
                               "TEST_repeating",
                               "TEST_longitudinal",
                               "TEST_multiarm")
#' @noRd
.blank_project <- list(
  short_name = NULL,
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
    data_extract_merged = NULL,
    project_type = "redcap",
    is_blank = TRUE,
    is_test = FALSE,
    ever_connected = FALSE,
    is_clean = FALSE,
    use_csv = FALSE
  ),
  links = list(
    redcap_uri = NULL,
    redcap_base = NULL,
    redcap_home = NULL,
    redcap_record_home = NULL,
    redcap_record_subpage = NULL,
    redcap_records_dashboard = NULL,
    redcap_api = NULL,
    redcap_api_playground = NULL,
    pkgdown = "https://thecodingdocs.github.io/REDCapSync/",
    github = "https://github.com/thecodingdocs/REDCapSync/",
    thecodingdocs = "https://www.thecodingdocs.com/"
  )
)
#' @noRd
set_dir <- function(dir_path) {
  dir_path <- clean_dir_path(dir_path)
  if (!file.exists(dir_path)) {
    if (utils::menu(
      choices = c("Yes", "No"),
      title = paste0(
        "No file path found for chosen directory, create? (",
        dir_path,
        ")"
      )
    ) == 1) {
      dir.create(file.path(dir_path))
    }
    if (!file.exists(dir_path)) {
      stop("Path not found. Use absolute path or choose one within R project working directory.")
    }
  }
  for (folder in .dir_folders) {
    if (!file.exists(file.path(dir_path, folder))) {
      dir.create(file.path(dir_path, folder), showWarnings = FALSE)
    }
  }
  assert_dir(dir_path, silent = FALSE)
}
#' @noRd
.dir_folders <- c("R_objects", "output", "scripts", "input", "REDCap")
#' @noRd
clean_dir_path <- function(dir_path) {
  if (!is.character(dir_path))
    stop("dir must be a character string")
  dir_path <- dir_path %>%
    trimws(whitespace = "[\\h\\v]") %>%
    sanitize_path()
  dir_path
}
