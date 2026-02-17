#' @title Get your Get Cache Path
#' @description
#' Included for transparency and confirmation/testing. This is where the basic
#' information about your projects is cached when you use the REDCapSync
#' package.
#' @details
#' This function checks the location of the cache established by
#' \code{\link[hoardr]{hoard}}.
#' \emph{No project data is stored here. Tokens are not stored here either.}
#' Key information stored here is `project_name` (primary key for REDCapSync
#' projects) and other details about project information.
#' @return The file path of your REDCapSync cache
#' @seealso
#' For more details, see \code{\link[hoardr]{hoard}}.
#' @examples
#' \dontrun{
#' path <- cache_path()
#' print(path)
#' }
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @export
cache_path <- function() {
  cache <- get_cache()
  path <- sanitize_path(cache$cache_path_get())
  path
}
#' @title Clear your cached projects
#' @description
#' Included for transparency and confirmation/testing.
#' @details
#' This function checks the location of the cache established by
#' \code{\link[hoardr]{hoard}} and deletes it!
#' This will not delete project data, just the packages stored "memory" of it.
#' @return messages confirming deleted cache
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @export
cache_clear <- function() {
  cache <- get_cache()
  cache$delete_all()
  cli_alert_warning("You must delete other files manually from the directory.")
  cli_alert_wrap(
    "REDCapSync cache cleared!",
    file = cache$cache_path_get(),
    bullet_type = "v"
  )
  invisible()
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
  cache_dir <- Sys.getenv("REDCAPSYNC_CACHE", unset = NA)
  if (!is.na(cache_dir) && nzchar(cache_dir)) {
    user_dir_exists <- dir.exists(cache_dir)
    if (!user_dir_exists) {
      check_directory_exists(cache_dir)
      cli_alert_warning(
        paste(
          "Cache directory via `Sys.getenv(\"REDCAPSYNC_CACHE\")`",
          "does not exist!"
        )
      )
    }
    if (user_dir_exists) {
      return(cache_dir)
    }
  }
  NULL
}
#' @title Remove project from cache
#' @inheritParams setup_project
#' @description
#' This will remove a project from the cache. The cache only stores information
#' like project_name, token_name, directory location, and details
#' from setup_project. If you want to truly delete the project files, you must
#' do so at the project directory you set up.
#' @return message of outcome
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @export
cache_remove_project <- function(project_name) {
  projects <- get_projects()
  is_in_cache <- project_name %in% projects$project_name
  if (!is_in_cache) {
    cli_alert_warning(
      paste0("'", project_name, "' is not saved in your cache. ",
             "Nothing to do. "))
    return(invisible())
  }
  projects <- projects[which(projects$project_name != project_name), ]
  save_projects_to_cache(projects, silent = TRUE)
  cli_alert_success(
    paste0("'", project_name, "' removed from cache, but its associated files",
    " are located at the directory you assigned to '", project_name, "' and",
    " must be deleted manually."
           ))
}
