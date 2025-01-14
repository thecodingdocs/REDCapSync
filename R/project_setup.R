#' @rdname setup-load
#' @title Setup or Load REDCapSync Project
#' @description
#' Setup or Load the `project` object for pipeline.
#'
#' @details
#' This function sets up the `project` object by storing the REDCap API token and
#' other configurations required for interacting with the REDCap server.
#' It ensures that the token is valid and ready for use in subsequent API calls.
#' Neither function directly attempts communication with REDCap.
#'
#' `setup_project` is used the first time you initialize/link a REDCap project.
#' Mainly, it sets your unique `short_name` and your intended directory.
#' Unless you run \code{reset = TRUE} the default will first try load_project.
#' dir_path is technically optional but without it the user cannot
#' save/load/update projects.
#'
#' `load_project` can be used with just the `short_name` parameter after you have
#' already run `setup_project` in the past with an established directory. `dir_path`
#' is optional for this function but can be used if you relocated the directory.
#'
#' @param short_name A character string with no spaces or symbols representing
#' the unique short name for the REDCap project.
#' @param dir_path Optional character string representing the directory path
#' where you want the REDCap project data to be stored. If missing, project object
#' will only be in current R session.
#' @param redcap_base A character string representing the base URL of the REDCap
#' server.
#' @param token_name An optional character string for setting your token name.
#' Default is `REDCapSync_<short_name>`
#' @param reset Logical (TRUE/FALSE). If TRUE, forces the setup even if the `project`
#' object already exists. Default is `FALSE`.
#' @param validate Logical (TRUE/FALSE). If TRUE, validates project object based on
#' current rules. Default is `TRUE`.
#' @param merge_form_name A character string representing the name of the merged
#' form. Default is "merged".
#' @param use_csv Logical (TRUE/FALSE). If TRUE, uses CSV files for data
#' storage. Default is `FALSE`.
#' @param auto_check_token Logical (TRUE/FALSE). If TRUE, automatically
#' checks the validity of the REDCap API token. Default is `TRUE`.
#' @param project_path A character string representing the file path of the exact
#' `<short_name>_REDCapSync.RData` file to be loaded.
#' @param with_data Logical (TRUE/FALSE). If TRUE, loads the test project object with
#' @param get_type optional character of REDCap API call type.
#' data as if user ran `sync_project`. Default is `FALSE`.
#' @param batch_size_download Integer. Number of records to process in each batch.
#' Default is `2000`.
#' @param batch_size_upload Integer. Number of records to process in each batch.
#' Default is `500`.
#' @param silent Logical (TRUE/FALSE). For messages.
#' @param labelled Logical (TRUE/FALSE). For whether or not to use labelled vs
#' raw coded data in output.
#' @return REDCapSync `project` list object.
#' @seealso
#' \code{\link[REDCapSync]{get_projects}} for retrieving a list of projects from
#' the directory cache.
#' @examplesIf FALSE
#' # Initialize the project object with the REDCap API token and URL
#' project <- setup_project(
#'   short_name = "TEST",
#'   dir_path = "path/to/secure/file/storage",
#'   redcap_base = "https://redcap.yourinstitution.edu/"
#' )
#' project <- load_project("TEST")
#' @family project object
#' @export
setup_project <- function(
    short_name,
    dir_path,
    redcap_base,
    token_name = paste0("REDCapSync_", short_name),
    labelled = TRUE,
    reset = FALSE,
    merge_form_name = "merged",
    use_csv = FALSE,
    get_type = c("identified","deidentified","strict-deidentified"),
    auto_check_token = TRUE,
    batch_size_download = 2000,
    batch_size_upload = 500,
    silent = FALSE
) {
  if(missing(redcap_base)){
    REDCapSync_REDCAP_BASE <- Sys.getenv("REDCapSync_REDCAP_BASE")
    if(is_something(REDCapSync_REDCAP_BASE)) {
      redcap_base <- validate_web_link(REDCapSync_REDCAP_BASE)
    }
  }
  em <- "`short_name` must be character string of length 1"
  if (!is.character(short_name)) stop(em)
  if (length(short_name) != 1) stop(em)
  projects <- get_projects() # add short_name conflict check id-base url differs
  short_name <- validate_env_name(short_name)
  if (paste0(internal_REDCapSync_token_prefix, short_name) != token_name) {
  } # maybe a message
  token_name <- validate_env_name(token_name)
  in_proj_cache <- short_name %in% projects$short_name
  missing_dir_path <- missing(dir_path)
  is_a_test <- is_test_short_name(short_name = short_name)
  was_loaded <- FALSE
  if (!reset) {
    has_expected_file <- FALSE
    if (!missing_dir_path || in_proj_cache) {
      if (missing_dir_path || in_proj_cache) {
        dir_path <- projects$dir_path[which(projects$short_name == short_name)]
      }
      expected_file <- file.path(
        dir_path,
        "R_objects",
        paste0(short_name, "_REDCapSync.RData")
      )
      has_expected_file <- file.exists(expected_file)
    }
    if (in_proj_cache || has_expected_file) {
      project <- load_project_from_path(expected_file) # if it fails should default to blank
      was_loaded <- TRUE
    }
    if (!(in_proj_cache || has_expected_file)) {
      if (is_a_test) {
        project <- load_test_project(short_name = short_name, with_data = FALSE)
      } else {
        project <- internal_blank_project
      }
      bullet_in_console(
        "Setup blank project object because nothing found in cache or directory.",
        bullet_type = "!",
        silent = silent
      )
    }
  }
  if (missing_dir_path) { # if missing the directory path from setup or load then let user know nothing will be stored
    if (!is_something(project$dir_path)) { # only show message if load_project wasn't used internally (that has a directory)
      bullet_in_console(
        "If you don't supply a directory, REDCapSync will only run in R session. Package is best with a directory.",
        bullet_type = "!",
        silent = silent
      )
    }
  }
  if (!missing_dir_path) {
    project$dir_path <- set_dir(dir_path) # will also ask user if provided dir is new or different (will load from original but start using new dir)
  }
  if(was_loaded) {
    #compare current setting to previous settings...
    if (!is.null(project$internals$data_extract_labelled)) {
      if (project$internals$data_extract_labelled != labelled) {
        if (!reset) {
          load_type <- ifelse(project$internals$data_extract_labelled, "labelled", "raw")
          chosen_type <- ifelse(labelled, "labelled", "raw")
          reset <- TRUE
          warning(
            "The project that was loaded was ",
            load_type, " and you chose ", chosen_type,
            ". Therefore, a full update was triggered to avoid data conflicts",
            immediate. = TRUE
          )
        }
      }
    }
  }
  if (reset) { # load blank if reset = TRUE
    project <- internal_blank_project
    bullet_in_console(
      paste0("Setup blank project object because `reset = TRUE`"),
      silent = silent
    )
  }
  project$short_name <- short_name
  project$internals$use_csv <- use_csv
  project$internals$data_extract_labelled <- labelled
  project$internals$batch_size_download <- batch_size_download %>% validate_integer()
  project$internals$batch_size_upload <- batch_size_upload %>% validate_integer()
  project$redcap$token_name <- token_name
  if (!is_a_test) {
    project$links$redcap_base <- validate_web_link(redcap_base)
    project$links$redcap_uri <- project$links$redcap_base %>% paste0("api/")
  } else {
    bullet_in_console(
      "Test objects ignore the `redcap_base` url argument and will not communicate with the REDCap API.",
      silent = silent
    )
  }
  project$internals$merge_form_name <- validate_env_name(merge_form_name)
  project$internals$use_csv <- use_csv
  project$internals$is_blank <- FALSE
  project$data <- project$data %>% all_character_cols_list()
  bullet_in_console(paste0("Token name: '", token_name, "'"))
  if (auto_check_token) {
    if (!is_valid_REDCap_token(validate_REDCap_token(project), is_a_test = is_a_test)) {
      set_REDCap_token(project, ask = FALSE)
    }
  }
  project <- validate_project(project, silent = silent)
  return(project)
}
#' @rdname setup-load
#' @export
load_project <- function(short_name, validate = TRUE) {
  projects <- get_projects()
  if (nrow(projects) == 0) stop("No projects in cache")
  if (!short_name %in% projects$short_name) stop("No project named ", short_name, " in cache. Did you use `setup_project()` and `sync_project()`?")
  dir_path <- projects$dir_path[which(projects$short_name == short_name)]
  if (!file.exists(dir_path)) stop("`dir_path` doesn't exist: '", dir_path, "'")
  project_path <- file.path(dir_path, "R_objects", paste0(short_name, "_REDCapSync.RData"))
  load_project_from_path(
    project_path = project_path,
    validate = validate
  ) %>% return()
}
#' @rdname setup-load
#' @export
load_project_from_path <- function(project_path, validate = TRUE) {
  if (!file.exists(project_path)) stop("No file at path '", project_path, "'. Did you use `setup_project()` and `sync_project()`?")
  project <- readRDS(file = project_path)
  # if failed through message like unlink to project_path
  if (validate) {
    project <- project %>% validate_project(silent = FALSE)
  }
  return(project)
}
#' @rdname setup-load
#' @export
load_test_project <- function(short_name = "TEST_repeating", with_data = FALSE) {
  em <- "`short_name` must be character string of length 1 equal to one of the following: " %>% paste0(as_comma_string(internal_allowed_test_short_names))
  if (!is.character(short_name)) stop(em)
  if (length(short_name) != 1) stop(em)
  if (!is_test_short_name(short_name = short_name)) stop(em)
  project <- internal_blank_project
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
  return(project)
}
#' @noRd
is_test_short_name <- function(short_name) {
  return(short_name %in% internal_allowed_test_short_names)
}
#' @noRd
is_test_project <- function(project) {
  return((project$short_name %in% internal_allowed_test_short_names) && project$internals$is_test)
}
#' @rdname save-deleteproject
#' @title Save or Delete project file from the directory
#' @param project A validated `project` object containing REDCap project data and
#' settings. Generated using \link{load_project} or \link{setup_project}
#' @inheritParams setup_project
#' @description
#' This will save/delete the "<short_name>_REDCapSync.RData" file in the given project
#' directories R_objects folder. These are optional functions given that
#' `save_project` is a also handled by a default parameter in `sync_project.`
#'
#' @details delete_project will not delete any other files from that directory. The
#' user must delete any other files manually.
#' @return Message
#' @family project object
#' @export
save_project <- function(project,silent = FALSE) {
  project <- project %>% validate_project()
  if (!project$internals$ever_connected) {
    bullet_in_console(
      paste0(
        "Did not save ",
        project$short_name,
        " because there has never been a REDCap connection! You must use `setup_project()` and `sync_project()`"
      ),
      bullet_type = "x",
      silent = silent
    )
    return(project)
  }
  # project <- reverse_clean_project(project) # # problematic because setting numeric would delete missing codes
  save_file_path <- file.path(
    project$dir_path,
    "R_objects",
    paste0(project$short_name, "_REDCapSync.RData")
  )
  saveRDS(
    object = project,
    file = save_file_path
  )# add error check
  bullet_in_console(
    paste0("Saved ", project$short_name ,"!"),
    bullet_type = "v",
    silent = silent
  )
  project$internals$last_directory_save <- Sys.time()
  add_project(project)
  # save_xls_wrapper(project)
  # nav_to_dir(project)
  return(project)
}
#' @rdname save-deleteproject
#' @export
delete_project <- function(project) {
  project <- validate_project(project)
  dir_path <- project$dir_path
  dir_path <- validate_dir(dir_path, silent = FALSE)
  delete_this <- file.path(dir_path, "R_objects", paste0(project$short_name, "_REDCapSync.RData"))
  if (file.exists(delete_this)) {
    unlink(delete_this)
    bullet_in_console("Deleted saved project", bullet_type = "v")
  } else {
    warning("The project object you wanted to is not there. Did you delete already? ", delete_this)
  }
}
#' @title get your directory
#' @inheritParams save_project
#' @return file path of directory
#' @export
get_dir <- function(project) {
  dir_path <- project$dir_path
  stop_mes <- "Did you use `set_dir()`?"
  if (!file.exists(dir_path)) {
    bullet_in_console("Searched for directory --> '", file = dir_path, bullet_type = "x")
    stop(paste0("Does not exist. ", stop_mes))
  }
  return(validate_dir(dir_path, silent = TRUE))
}
#' @title nav_to_dir
#' @inheritParams save_project
#' @return opens browser link
#' @export
nav_to_dir <- function(project) {
  utils::browseURL(project$dir_path)
}
#' @noRd
validate_project <- function(project, silent = TRUE, warn_only = FALSE) {
  # param check
  if (!is.list(project)) stop("project must be a list")
  # function
  outcome_valid <- TRUE
  messages <- NULL
  if (!all(names(internal_blank_project) %in% names(project))) {
    outcome_valid <- FALSE
    messages <- messages %>% append("`project` does not have the appropriate names. Did you use `load_project()` or `setup_project()` to generate it?")
  }
  if (is.null(project$short_name)) {
    outcome_valid <- FALSE
    messages <- messages %>% append("`project$short_name` is NULL!, Did you use `setup_project()`?")
  } else {
    project$short_name %>% validate_env_name()
  }
  if (!silent) {
    if ((length(project$data) == 0) > 0) {
      bullet_in_console("Valid project object but no data yet!", bullet_type = "!")
    }
    if (is.null(project$dir_path)) {
      bullet_in_console("`project$dir_path` is NULL!, Did you use `setup_project()`?", bullet_type = "!")
    } else {
      if (!project$dir_path %>% file.exists()) {
        bullet_in_console(paste0("`project$dir_path`, '", project$dir_path, "', does not exist!, Did you use `setup_project()`?\nThis can also happen with shared directories."), bullet_type = "!")
      } else {
        bullet_in_console(project$short_name %>% paste0(" loaded from: "), url = project$dir_path, bullet_type = "v")
      }
    }
    if ((project$internals$is_test)) {
      bullet_in_console(project$short_name %>% paste0(" is a test project object that doesn't actually communicate with any REDCap API!"), bullet_type = "i")
    }
    if (project$internals$is_transformed) {
      bullet_in_console(project$short_name %>% paste0(" is currently transformed! Can reverse with `untransform_project(project)`"), bullet_type = "i")
    }
    bullet_in_console("To get data/updates from REDCap run `project <- sync_project(project)`")
  }
  if (outcome_valid) {
    bullet_in_console(paste0(project$short_name, " is valid project object!"), bullet_type = "v")
  }
  if (!outcome_valid) {
    for (m in messages) {
      if (warn_only) {
        warning(m, immediate. = TRUE)
      } else {
        stop(m)
      }
    }
  }
  return(project)
}
#' @noRd
internal_allowed_test_short_names <- c("TEST_classic", "TEST_repeating", "TEST_longitudinal", "TEST_multiarm")
#' @noRd
internal_blank_project <- list(
  short_name = NULL,
  dir_path = NULL,
  redcap = list(
    token_name = NULL,
    project_id = NULL,
    project_title = NULL,
    id_col = NULL,
    version = NULL,
    project_info = NULL,
    log = NULL,
    users = NULL,
    current_user = NULL,
    choices = NULL,
    raw_structure_cols = NULL,
    is_longitudinal = NULL,
    has_arms = NULL,
    has_multiple_arms = NULL,
    has_arms_that_matter = NULL,
    has_repeating_forms_or_events = NULL,
    has_repeating_forms = NULL,
    has_repeating_events = NULL
  ),
  metadata = list( # model
    forms = NULL,
    fields = NULL,
    choices = NULL,
    form_key_cols = NULL,
    arms = NULL,
    events = NULL,
    event_mapping = NULL,
    missing_codes = NULL
  ),
  data = NULL, # model
  data_update = NULL,
  quality_checks = NULL,
  transformation = list(
    forms = NULL,
    fields = NULL,
    field_functions = NULL,
    original_forms = NULL,
    original_fields = NULL,
    data_updates = NULL
  ),
  summary = list(
    subsets = NULL
  ),
  internals = list(
    last_test_connection_attempt = NULL,
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
    data_extract_labelled = NULL,
    data_extract_merged = NULL,
    merge_form_name = "merged",
    project_type = "redcap",
    is_blank = TRUE,
    is_test = FALSE,
    ever_connected = FALSE,
    is_transformed = FALSE,
    is_clean = FALSE,
    use_csv = FALSE
  ),
  links = list(
    redcap_base = NULL,
    redcap_uri = NULL,
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
    if (utils::menu(choices = c("Yes", "No"), title = paste0("No file path found for chosen directory, create? (", dir_path, ")")) == 1) {
      dir.create(file.path(dir_path))
    }
    if (!file.exists(dir_path)) {
      stop("Path not found. Use absolute path or choose one within R project working directory.")
    }
  }
  for (folder in internal_dir_folders) {
    if (!file.exists(file.path(dir_path, folder))) {
      dir.create(file.path(dir_path, folder), showWarnings = FALSE)
    }
  }
  return(validate_dir(dir_path, silent = FALSE))
}
#' @noRd
internal_dir_folders <- c("R_objects", "output", "scripts", "input", "REDCap")
#' @noRd
validate_dir <- function(dir_path, silent = TRUE) {
  # param check
  dir_path <- clean_dir_path(dir_path)
  if (!file.exists(dir_path)) stop("dir_path does not exist")
  if (!is.logical(silent)) stop("silent parameter must be TRUE/FALSE")
  stop_mes <- "Did you use `setup_project()`?"
  for (folder in internal_dir_folders) {
    if (!file.exists(file.path(dir_path, folder))) stop("'", dir_path, "/", folder, "' missing! ", stop_mes)
  }
  # if ( ! file.exists(file.path(dir_path,"ref_tables"))) stop("'",dir_path,"/ref_tables' missing! ",stop_mes)
  if (!silent) bullet_in_console("Directory is Valid!", url = dir_path, bullet_type = "v")
  dir_path
}
#' @noRd
clean_dir_path <- function(dir_path) {
  if (!is.character(dir_path)) stop("dir must be a character string")
  dir_path <- dir_path %>%
    trimws(whitespace = "[\\h\\v]") %>%
    sanitize_path()
  return(dir_path)
}
