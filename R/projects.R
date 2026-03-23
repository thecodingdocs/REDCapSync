#' @title Manage REDCapSync projects
#' @description
#' `projects` is the central interface for managing REDCap projects in
#' **REDCapSync**. It provides a convenient, persistent registry of projects
#' that can be accessed across R sessions.
#'
#' Each time a project is set up or synced, basic metadata is stored locally so
#' that projects can be reloaded and updated without reconfiguration.
#'
#' @details
#' `projects` is implemented as a singleton R6 object and serves as the main
#' entry point to the REDCapSync workflow. All project-level operations—such as
#' setup, loading, syncing, and removal—are accessed through this object.
#'
#' A key advantage of this design is support for **method chaining**. Because
#' project methods return project objects, you can write concise, readable
#' workflows that operate in sequence:
#'
#' \preformatted{
#' projects$load("my_project")$sync()$save_datasets()
#' }
#'
#' @details
#' The default location of the cache location is defined by using
#' R_USER_CACHE_DIR if set. Otherwise, it follows platform conventions via
#' [hoardr], saving a file "R/REDCapSync/projects.rds". No direct project data
#' is stored in the cache. Notably, tokens and data are not stored here. The key
#' variables stored in the cache are...
#' * `project_name` - unique identifier for REDCapSync package
#' * `redcap_uri` - server location
#' * `token_name` - where to find token environment with [Sys.getenv()]
#' * `dir_path` - where to saved project and associated files locally
#' * `project_id` - obtained from API call and "locks-in" the connection
#' * `redcap_version` - obtained from API call and affects links
#' * `last_sync` and `sync_frequency` - informs REDCap sync of when to update
#' * other variables from project info and some internal package mechanics
#' @family Cache Functions
#' @keywords Cache
#' @seealso \code{vignette("Cache", package = "REDCapSync")}
#' @returns R6 object that can used be to access project objects
#' @export
projects <- R6::R6Class(
  "REDCapSyncProjects",
  public = list(
    initialize = function() {
      invisible(self)
    },
    print = function() {
      private$project_df <- get_projects()
      private$n_projects <- nrow(private$project_df)
      cli_h1("REDCapSync")
      cli_text("{private$n_projects} REDCap Projects!")
      if (private$n_projects > 0L) {
        number_due <- private$project_df$project_name |>
          lapply(due_for_sync) |>
          unlist()
        cli_text("{number_due} due for sync!")
      }
      invisible(self)
    },
    df = function() {
      private$project_df <- get_projects()
      private$n_projects <- nrow(private$project_df)
      private$project_df
    },
    n = function() {
      private$project_df <- get_projects()
      private$n_projects <- nrow(private$project_df)
      private$n_projects
    },
    test_project_names = function() {
      .test_project_names
    },
    load = function(project_name) {
      load_project(project_name)
    },
    setup = function(project_name,
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
                     get_users = TRUE,
                     get_data = TRUE,
                     batch_size_download = 1000L,
                     batch_size_upload = 500L,
                     get_entire_log = FALSE,
                     log_days = 10L,
                     log_drop_details = FALSE,
                     log_drop_exports = FALSE,
                     timezone = Sys.timezone(),
                     get_files = FALSE,
                     get_file_repository = FALSE,
                     original_file_names = FALSE,
                     add_default_datasets = TRUE) {
      args <- list(
        project_name = project_name,
        token_name = token_name,
        sync_frequency = sync_frequency,
        labelled = labelled,
        hard_reset = hard_reset,
        get_type = get_type,
        records = records,
        fields = fields,
        forms = forms,
        events = events,
        filter_logic = filter_logic,
        get_users = get_users,
        get_data = get_data,
        batch_size_download = batch_size_download,
        batch_size_upload = batch_size_upload,
        get_entire_log = get_entire_log,
        log_days = log_days,
        log_drop_details = log_drop_details,
        log_drop_exports = log_drop_exports,
        timezone = timezone,
        get_files = get_files,
        get_file_repository = get_file_repository,
        original_file_names = original_file_names,
        add_default_datasets = add_default_datasets
      )
      if (!missing(dir_path)) {
        args$dir_path <- dir_path
      }
      if (!missing(redcap_uri)) {
        args$redcap_uri <- redcap_uri
      }
      do.call(setup_project, args)
    },
    test_tokens = function(offline = config$offline()) {
      if (offline) {
        token_check()
      }
      # loop now
    },
    remove = function(project_names) {
      cache_clear(project_names = project_names)
      invisible(self)
    },
    remove_all = function() {
      cache_clear()
      invisible(self)
    },
    sync = function(project_names = NULL,
                    save_datasets = TRUE,
                    hard_check = FALSE,
                    hard_reset = FALSE) {
      sync(project_names = project_names,
           save_datasets = save_datasets,
           hard_check = hard_check,
           hard_reset = hard_reset)
    }
  ),
  private = list(
    project_df = NULL,
    n_projects = NULL
  ),
  cloneable = FALSE
)$new()
#' @noRd
get_projects <- function() {
  does_exist <- FALSE
  if (file.exists(cache_path())) {
    does_exist <- cache_path() |> file.path("projects.rds") |> file.exists()
    if (!does_exist) {
      cli_alert_warning("No cached projects... use `setup_project(...)`")
    }
  }
  does_exist
  is_ok <- FALSE
  if (does_exist) {
    projects <- cache_path() |> file.path("projects.rds") |> readRDS()
    # add try catch?
    is_ok <- test_project_details(projects)
    if (!is_ok) {
      projects <- repair_projects(projects)
      is_ok <- !is.null(projects)
    }
    if (!is_ok) {
      warning_message <- paste0("You have projects cached, but due to a ",
                                "version change or other issue, it has to be ",
                                "cleared. Use `setup_project(...)`")
      cli_alert_warning(warning_message)
      cache_clear()
    }
  }
  if (!is_ok) {
    # message here?
    return(.blank_project_details)
  }
  # message here? would have to silence internally
  projects # invisible?
}
#' @noRd
.blank_project_cols <- c(
  "project_name",
  "dir_path",
  "sync_frequency",
  "last_sync",
  "timezone",
  "last_directory_save",
  "last_metadata_update",
  "last_data_update",
  "version",
  "token_name",
  "project_id",
  "project_title",
  "labelled",
  "id_col",
  "is_longitudinal",
  "has_repeating_forms_or_events",
  "has_multiple_arms",
  "object_size",
  # "file_size",
  "n_records",
  "redcap_uri",
  # "redcap_base",
  "redcap_home",
  # "redcap_api_playground",
  # "log_days",
  "get_files",
  "get_file_repository",
  # "original_file_names",
  "get_entire_log",
  "get_data",
  "get_type"
)
#' @noRd
.blank_project_details <- data.frame(
  project_name = character(0L),
  dir_path = character(0L),
  sync_frequency = character(0L),
  last_sync = character(0L) |> as.POSIXct(tz = Sys.timezone()),
  timezone = character(0L),
  last_directory_save = character(0L) |> as.POSIXct(tz = Sys.timezone()),
  last_metadata_update = character(0L) |> as.POSIXct(tz = Sys.timezone()),
  last_data_update = character(0L) |> as.POSIXct(tz = Sys.timezone()),
  version = character(0L),
  token_name = character(0L),
  project_id = character(0L),
  project_title = character(0L),
  labelled = logical(0L),
  id_col = character(0L),
  is_longitudinal = logical(0L),
  has_repeating_forms_or_events = logical(0L),
  has_multiple_arms = logical(0L),
  object_size = character(0L),
  n_records = integer(0L),
  redcap_uri = character(0L),
  redcap_home = character(0L),
  get_files = logical(0L),
  get_file_repository = logical(0L),
  get_entire_log = logical(0L),
  get_data = logical(0L),
  get_type = character(0L),
  stringsAsFactors = FALSE
)
#' @noRd
save_projects_to_cache <- function(projects, silent = TRUE) {
  assert_project_details(projects)
  new_order <- order(projects$project_name)
  projects <- projects[new_order, ]
  saveRDS(projects, file = cache_path() |> file.path("projects.rds"))
  pkg_name <- "REDCapSync"
  if (!silent) {
    cli_alert_success(
      paste0(
        pkg_name,
        " saved ",
        nrow(projects),
        " project locations to the cache...",
        toString(projects$project_name)
      ) # "   Token: ",projects$token_name,collapse = "\n"))
    )
  }
}
#' @noRd
extract_project_details <- function(project) {
  assert_setup_project(project)
  project_details <- matrix(
    data = NA,
    ncol = length(.blank_project_cols),
    nrow = 1L,
    dimnames = list(NULL, .blank_project_cols)
  ) |> as.data.frame()
  # top -----
  project_details$project_name <- project$project_name
  project_details$dir_path <- na_if_null(project$dir_path)
  # settings -------
  project_details$sync_frequency <- project$settings$sync_frequency
  project_details$get_files <- project$settings$get_files
  project_details$get_file_repository <- project$settings$get_file_repository
  project_details$get_entire_log <- project$settings$get_entire_log
  project_details$get_data <- project$settings$get_data
  project_details$labelled <- project$settings$labelled
  project_details$get_type <- project$settings$get_type
  # redcap --------
  project_details$version <- na_if_null(project$redcap$version)
  project_details$token_name <- na_if_null(project$token_name)
  project_details$project_id <- na_if_null(project$redcap$project_id)
  project_details$project_title <- na_if_null(project$redcap$project_title)
  project_details$id_col <- na_if_null(project$metadata$id_col)
  project_details$is_longitudinal <-
    na_if_null(project$metadata$is_longitudinal)
  project_details$has_repeating_forms_or_events <-
    na_if_null(project$metadata$has_repeating_forms_or_events)
  project_details$has_multiple_arms <-
    na_if_null(project$metadata$has_multiple_arms)
  project_details$n_records <- project$record_summary |>
    nrow() |>
    na_if_null() |>
    as.integer()
  project_details$redcap_uri <- project$links$redcap_uri
  project_details$redcap_home <- project$links$redcap_home |>
    na_if_null() |>
    as.character()
  project_details$timezone <- project$redcap$timezone |>
    na_if_null() |>
    as.character()
  # saving ----
  project_details$last_sync <- project$internals$last_sync |>
    na_if_null() |>
    as.POSIXct(tz = Sys.timezone())
  project_details$last_directory_save <-
    project$internals$last_directory_save |>
    na_if_null() |>
    as.POSIXct(tz = Sys.timezone())
  project_details$last_metadata_update <-
    project$internals$last_metadata_update |>
    na_if_null() |>
    as.POSIXct(tz = Sys.timezone())
  project_details$last_data_update <- project$internals$last_data_update |>
    na_if_null() |>
    as.POSIXct(tz = Sys.timezone())
  project_details$object_size <- object_size(project) |> as.character()
  project_details
}
#' @noRd
add_project_details_to_cache <- function(project_details) {
  assert_project_details(project_details, nrows = 1L)
  projects <- get_projects()
  proj_row_diff <- which(projects$project_name != project_details$project_name)
  projects <- projects[proj_row_diff, ]
  bad_row <- NULL
  if (!is.na(project_details$project_id)) {
    bad_row <- which(
      projects$project_id == project_details$project_id &
        dirname(projects$redcap_uri) == dirname(project_details$redcap_uri)
    )
  }
  if (length(bad_row) > 0L) {
    cli_abort(
      paste0(
        "You are trying to save from a project [{project_details$project_name}",
        " PID {projects$project_id[bad_row]}] that you have already setup ",
        "[{projects$project_name[bad_row]} PID {project_details$project_id}] ",
        "You can load the old project or run ",
        "`cache_clear(\"{projects$project_name[bad_row]}\")`"
      )
    )
  }
  projects <- bind_rows(projects, project_details)
  save_projects_to_cache(projects)
}
