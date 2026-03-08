#' @title Clear your cached projects
#' @description
#' Finds the location of the cache established by \code{\link[hoardr]{hoard}}
#' and deletes stored project information (not data)! If you provide
#' `project_names`, it will remove only those projects from the cache.
#' @details
#' The cache only stores information like project_name, token_name, directory
#' location, and other details from [setup_project()]. If you want to truly delete
#' the project files, you must do so at the project directory you set up.
#' @inheritParams sync
#' @examples
#' \dontrun{
#' cache_clear("OLD_PROJECT")
#' cache_clear() # every project
#' }
#' @family Cache Functions
#' @keywords Cache Functions
#' @seealso \code{vignette("Cache", package = "REDCapSync")}
#' @return message of outcome
#' @export
cache_clear <- function(project_names = NULL) {
  cache <- get_cache()
  if (!is.null(project_names)) {
    for (project_name in project_names) {
      cache_remove_project(project_name)
    }
    return(invisible())
  }
  cache$delete_all()
  cli_alert_warning("You must delete any files manually from the directory.")
  cli_alert_wrap(
    "REDCapSync cache cleared!",
    file = cache$cache_path_get(),
    bullet_type = "v"
  )
  invisible()
}
#' @noRd
cache_remove_project <- function(project_name) {
  assert_env_name(project_name, max.chars = 31L, all_caps = TRUE)
  projects <- get_projects()
  is_in_cache <- project_name %in% projects$project_name
  if (!is_in_cache) {
    cli_alert_warning("'{project_name}' is not in your cache. Nothing to do.")
    return(invisible())
  }
  projects <- projects[which(projects$project_name != project_name), ]
  save_projects_to_cache(projects, silent = TRUE)
  success_message <- paste0("'{project_name}' removed from cache, but files",
                            " may still be located in the directory.")
  cli_alert_success(success_message)
  invisible()
}
#' @noRd
get_cache <- function() {
  cache <- hoardr::hoard()
  cache$cache_path_set(path = "REDCapSync", type = "user_cache_dir")
  cache_dir <- get_cache_dir()
  if (!is.null(cache_dir)) {
    full_path <- file.path(cache_dir, ".cache")
    dir.create(full_path, showWarnings = FALSE)
    cache$cache_path_set(full_path = full_path)
  }
  cache$mkdir()
  cache
}
#' @noRd
get_cache_dir <- function() {
  cache_dir <- Sys.getenv("REDCAPSYNC_CACHE_OVERRIDE", unset = NA)
  if (!is.na(cache_dir) && nzchar(cache_dir)) {
    user_dir_exists <- dir.exists(cache_dir)
    if (!user_dir_exists) {
      check_directory_exists(cache_dir)
      warning_message <- paste("Cache directory via",
                               "`Sys.getenv(\"REDCAPSYNC_CACHE_OVERRIDE\")`",
                               "does not exist!")
      cli_alert_warning(warning_message)
    }
    if (user_dir_exists) {
      return(cache_dir)
    }
  }
  NULL
}
#' @noRd
cache_exists <- function() {
  cache <- get_cache()
  file.exists(cache$cache_path_get())
}
#' @noRd
cache_projects_exists <- function() {
  does_exist <- FALSE
  if (cache_exists()) {
    does_exist <- cache_path() |>
      file.path("projects.rds") |>
      file.exists()
    if (!does_exist) {
      cli_alert_warning("No cached projects... use `setup_project(...)`")
    }
  }
  does_exist
}
#' @noRd
cache_path <- function() {
  cache <- get_cache()
  path <- sanitize_path(cache$cache_path_get())
  path
}
