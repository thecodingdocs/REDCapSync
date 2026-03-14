#' @title Get your REDCap projects used by REDCapSync
#' @description
#' Everytime a project is synced, basic project information is saved to the
#' package user cache so that [load_project] and [sync] work across R sessions.
#' @details
#' No direct project data is stored in the cache. Notably, tokens and data are
#' not stored here. The key variables stored in the cache are...
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
#' @returns data.frame of cached projects
#' @export
get_projects <- function() {
  does_exist <- cache_projects_exists()
  is_ok <- FALSE
  if (does_exist) {
    projects <-  cache_path() |>
      file.path("projects.rds") |>
      readRDS() # add try catch?
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
  "R_object_size",
  # "file_size",
  "n_records",
  "redcap_uri",
  # "redcap_base",
  "redcap_home",
  # "redcap_api_playground",
  # "days_of_log",
  "get_files",
  "get_file_repository",
  # "original_file_names",
  "entire_log",
  "metadata_only",
  "get_type"
  # "batch_size_download",
  # "batch_size_upload"
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
  R_object_size = character(0L),
  n_records = integer(0L),
  redcap_uri = character(0L),
  redcap_home = character(0L),
  get_files = logical(0L),
  get_file_repository = logical(0L),
  entire_log = logical(0L),
  metadata_only = logical(0L),
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
    cli_alert_wrap(
      text = paste0(
        "Cache is stored in directory on your computer. It can be found with `",
        pkg_name,
        "::cache_path()`, and cleared with `",
        pkg_name,
        "::cache_clear()`."
      ),
      file = cache_path()
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
  project_details$sync_frequency <- project$internals$sync_frequency
  project_details$get_files <- project$internals$get_files
  project_details$get_file_repository <- project$internals$get_file_repository
  project_details$entire_log <- project$internals$entire_log
  project_details$metadata_only <- project$internals$metadata_only
  project_details$labelled <- project$internals$labelled
  project_details$get_type <- project$internals$get_type
  project_details$timezone <- project$internals$timezone |>
    na_if_null() |>
    as.character()
  # redcap --------
  project_details$version <- na_if_null(project$redcap$version)
  project_details$token_name <- na_if_null(project$redcap$token_name)
  project_details$project_id <- na_if_null(project$redcap$project_id)
  project_details$project_title <- na_if_null(project$redcap$project_title)
  project_details$id_col <- na_if_null(project$metadata$id_col)
  project_details$is_longitudinal <-
    na_if_null(project$metadata$is_longitudinal)
  project_details$has_repeating_forms_or_events <-
    na_if_null(project$metadata$has_repeating_forms_or_events)
  project_details$has_multiple_arms <-
    na_if_null(project$metadata$has_multiple_arms)
  project_details$n_records <- project$summary$all_records |>
    nrow() |>
    na_if_null() |>
    as.integer()
  project_details$redcap_uri <- project$links$redcap_uri
  project_details$redcap_home <- project$links$redcap_home |>
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
  project_details$R_object_size <- object_size(project) |> as.character()
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
