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
#' @param days_of_log Integer. Number of days to be checked in the log if a reset
#' or new project is setup. Default is `10`.
#' @param get_files Logical (TRUE/FALSE). If TRUE, retrieves files from REDCap.
#' Default is `FALSE`.
#' @param get_file_repository Logical (TRUE/FALSE). If TRUE, retrieves file repository
#' from REDCap.
#' Default is `FALSE`.
#' @param original_file_names Logical (TRUE/FALSE). If TRUE, uses original file
#' names for retrieved files. Default is `FALSE`.
#' @param entire_log Logical (TRUE/FALSE). If TRUE, retrieves the entire log.
#' Default is `FALSE`.
#' @param metadata_only Logical (TRUE/FALSE). If TRUE, updates only the
#' metadata. Default is `FALSE`.
#' @param auto_check_token Logical (TRUE/FALSE). If TRUE, automatically
#' checks the validity of the REDCap API token. Default is `TRUE`.
#' @param project_path A character string representing the file path of the exact
#' `<short_name>_REDCapSync.RData` file to be loaded.
#' @param with_data Logical (TRUE/FALSE). If TRUE, loads the test project
#' object with data included.
#' @param get_type optional character of REDCap API call type.
#' data as if user ran `sync_project`. Default is `FALSE`.
#' @param batch_size_download Integer. Number of records to process in each
#' batch.
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
    reset = FALSE,
    labelled = TRUE,
    days_of_log = 10,
    get_files = FALSE,
    get_file_repository = FALSE,
    original_file_names = FALSE,
    entire_log = FALSE,
    metadata_only = FALSE,
    merge_form_name = "merged",
    use_csv = FALSE,
    get_type = "identified",
    auto_check_token = TRUE,
    batch_size_download = 2000,
    batch_size_upload = 500,
    silent = FALSE
) {
  project$short_name <- assert_env_name(
    env_name = short_name,
    max.chars = 31,
    arg_name = "short_name"
  )
  assert_character(
    merge_form_name,
    len = 1,
    min.chars = 1,
    .var.name = "merge_form_name"
  )
  assert_choice(
    get_type,
    choices = c("identified", "deidentified", "strict-deidentified")
  )
  assert_logical(reset, len = 1)
  assert_logical(labelled, len = 1)
  assert_integerish(days_of_log, len = 1, lower = 1)
  assert_logical(get_files, len = 1)
  assert_logical(get_file_repository, len = 1)
  assert_logical(original_file_names, len = 1)
  assert_logical(entire_log, len = 1)
  assert_logical(metadata_only, len = 1)
  assert_env_name(merge_form_name,arg_name = "merge_form_name")
  assert_logical(use_csv, len = 1)
  assert_logical(auto_check_token, len = 1)
  assert_integerish(batch_size_download, len = 1, lower = 1)
  assert_integerish(batch_size_upload, len = 1, lower = 1)
  assert_logical(silent, len = 1)
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
  short_name <- assert_env_name(short_name)
  if (paste0(internal_REDCapSync_token_prefix, short_name) != token_name) {
  } # maybe a message
  token_name <- assert_env_name(token_name)
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
  if(was_loaded) {
    #compare current setting to previous settings...
    if (!is.null(project$internals$labelled)) {
      if (project$internals$labelled != labelled) {
        if (!reset) {
          load_type <- ifelse(project$internals$labelled, "labelled", "raw")
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
  if (!missing_dir_path) {
    project$dir_path <- set_dir(dir_path) # will also ask user if provided dir is new or different (will load from original but start using new dir)
  }
  project$short_name <- short_name
  project$internals$use_csv <- use_csv
  project$internals$labelled <- labelled
  project$internals$original_file_names <- original_file_names
  project$internals$entire_log <- entire_log
  project$internals$days_of_log <- days_of_log
  project$internals$get_files <- get_files
  project$internals$metadata_only <- metadata_only
  project$internals$batch_size_download <- batch_size_download %>% assert_integerish()
  project$internals$batch_size_upload <- batch_size_upload %>% assert_integerish()
  project$redcap$token_name <- token_name
  project$internals$get_file_repository <- get_file_repository
  if (!is_a_test) {
    project$links$redcap_base <- validate_web_link(redcap_base)
    project$links$redcap_uri <- project$links$redcap_base %>% paste0("api/")
  } else {
    bullet_in_console(
      "Test objects ignore the `redcap_base` url argument and will not communicate with the REDCap API.",
      silent = silent
    )
  }
  project$internals$merge_form_name <- assert_env_name(merge_form_name)
  project$internals$use_csv <- use_csv
  project$internals$is_blank <- FALSE
  project$data <- project$data %>% all_character_cols_list()
  bullet_in_console(paste0("Token name: '", token_name, "'"))
  if (auto_check_token) {
    if (!is_valid_REDCap_token(validate_REDCap_token(project), is_a_test = is_a_test)) {
      set_REDCap_token(project, ask = FALSE)
    }
  }
  project <- assert_project(project, silent = silent)
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
    project <- project %>% assert_project(silent = FALSE)
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
  project <- project %>% assert_project()
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
  project <- assert_project(project)
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
assert_collection <- function(collection){
  assert_list(collection, any.missing = FALSE, len = 3, names = "unique")
  assert_names(names(collection), identical.to = names(makeAssertCollection()))
  return(invisible(collection))
}
#' @noRd
cli_message_maker <- function(collected, function_name, info, internal = TRUE) {
  assert_collection(collected)
  assert_character(function_name, any.missing = FALSE, len = 1)
  assert_logical(internal, any.missing = FALSE, len = 1)
  if (internal) {
    message <- c("i" = "This is an internal function. Something is very wrong!")
  } else {
    pkg_separator <- ifelse(internal, ":::", "::")
    pkg_ref <- paste0("{.fun REDCapSync", pkg_separator, function_name, "}")
    message <- c("i" = paste0("See ", pkg_ref, " or github page for help."))
  }
  if (!missing(info)) {
    assert_character(info, min.len = 1)
    names(info) <- rep_len("i", length.out = length(info))
    message <- message %>% append(info)
  }
  mistakes <- collected$getMessages()
  names(mistakes) <- rep_len(">", length.out = length(mistakes))
  message <- message %>% append(mistakes)
  return(message)
}
#' @noRd
is_exported <- function(func_name) {
  # # Check if the package namespace is loaded
  #
  # # Get the namespace environment for the package
  # ns_env <- asNamespace("REDCapSync")
  # "setup_project" %in% getNamespaceExports(ns_env)
  # RosyDev::get_external_functions()
  # getNamespaceImports()
}
#' @noRd
assert_project <- function(
    project,
    silent = TRUE,
    warn_only = FALSE,
    add = NULL
) {
  standalone <- is.null(add)
  if ( ! standalone) {
    assert_collection(add)
  }
  collected <- makeAssertCollection()
  assert_logical(silent, any.missing = FALSE, len = 1, add = collected)
  assert_logical(warn_only, any.missing = FALSE, len = 1, add = collected)
  current_function <- as.character(current_call())[[1]]
  if( ! collected$isEmpty()){
    message <- collected %>% cli_message_maker(function_name = current_function)
    cli::cli_abort(message)
  }
  collected <- makeAssertCollection()
  assert_list(
    project,
    names = "unique",
    len = length(internal_blank_project),
    add = collected
  )
  assert_names(
    names(internal_blank_project),
    type = "unique",
    identical.to = names(internal_blank_project),
    add = collected
  )
  assert_env_name(project$short_name,add = collected)
  assert_logical(project$internals$reset, len = 1,add = collected)
  assert_logical(project$internals$labelled, len = 1,add = collected)
  assert_integerish(project$internals$days_of_log, len = 1, lower = 1,add = collected)
  assert_logical(project$internals$get_files, len = 1,add = collected)
  assert_logical(project$internals$get_file_repository, len = 1,add = collected)
  assert_logical(project$internals$original_file_names, len = 1,add = collected)
  assert_logical(project$internals$entire_log, len = 1,add = collected)
  assert_logical(project$internals$metadata_only, len = 1,add = collected)
  assert_env_name(
    project$internals$merge_form_name,
    max.chars = 31,
    arg_name = "merge_form_name",
    add = collected
  )
  assert_logical(project$internals$use_csv, len = 1,add = collected)
  assert_logical(project$internals$auto_check_token, len = 1,add = collected)
  assert_integerish(project$internals$batch_size_download, len = 1, lower = 1,add = collected)
  assert_integerish(project$internals$batch_size_upload, len = 1, lower = 1,add = collected)
  assert_logical(project$internals$silent, len = 1,add = collected)
  if(! collected$isEmpty()) {
    if (standalone){
      collected %>%
        cli_message_maker(function_name = current_function) %>%
        cli::cli_abort(message)
    }else{
      add$push(
        cli::format_message(
          paste0(
            "Did you use {.fun REDCapSync::setup_project}? ",
            "Consider using `reset = TRUE`."
          )
        )
      )
    }
  }
  return(invisible(project))
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
    get_files = NULL,
    get_file_repository = NULL,
    original_file_names = NULL,
    days_of_log = NULL,
    entire_log = NULL,
    labelled = NULL,
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
