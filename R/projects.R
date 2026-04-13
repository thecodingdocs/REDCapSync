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
#' `projects` is implemented as a singleton list object and serves as a single
#' entry point to the REDCapSync workflow. All project-level operations—such as
#' setup, loading, syncing, and removal—are accessed through this object.
#'
#' A key advantage of this design is support for **method chaining**. Because
#' many project methods return project objects, you can write concise, readable
#' workflows that operate in sequence:
#'
#' \preformatted{
#' # load previous project and sync with API
#' project <- REDCapSync::projects$load("TEST_CLASSIC")$sync()
#' # load dataset to global envir based on custom filters
#' dataset <- project$generate_dataset(envir = globalenv(),
#'                                     filter_field = "var_branching",
#'                                     filter_choices = "Yes")
#' }
#'
#' @family Cache Functions
#' @keywords Cache
#' @seealso \code{vignette("Cache", package = "REDCapSync")}
#' @examples
#' project_df <- projects$df
#' project$print()
#' project$any()
#' project$n()
#' TEST_CLASSIC <- project$load("TEST_CLASSIC")
#'
#' @returns list of functions used as single entry into REDCapSync package
#' @export
projects <- list(
  print = function() {
    project_df <- get_projects()
    n_projects <- nrow(project_df)
    cli_h1("REDCapSync")
    cli_h2("Your Projects (n = {n_projects})")
    if (n_projects > 0L) {
      number_due <- project_df$project_name |>
        lapply(due_for_sync) |>
        unlist() |>
        which() |>
        length()
      cli_text("{toString(project_df$project_name)}")
      cli_alert_info("{number_due} due for sync!")
    }
    cli_h2("Test Projects (n = {length(.test_project_names)})")
    cli_text("{toString(.test_project_names)}")
    help_cli_text()
    invisible(project_df)
  },
  df = function() {
    get_projects()
  },
  n = function() {
    nrow(get_projects())
  },
  any = function() {
    nrow(get_projects()) > 0L
  },
  load = function(project_name) {
    load_project(project_name)
  }
)
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
#' @noRd
help_cli_text <- function() {
  cli_h2("Help")
  pkgdown_link <- .pkgdown_link
  github_link <- .github_link
  cli_text("Pkgdown: {.url {pkgdown_link}}")
  cli_text("Github: {.url {github_link}}")
  cli_text("Datasets: {.vignette REDCapSync::Datasets}")
  cli_text("Tokens: {.vignette REDCapSync::Tokens}")
}
#' @noRd
.pkgdown_link <- "https://thecodingdocs.github.io/REDCapSync/"
#' @noRd
.github_link <- "https://github.com/thecodingdocs/REDCapSync/"
