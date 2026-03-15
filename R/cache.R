#' @title Clear your cached projects
#' @description
#' Finds the location of the cache established by \code{\link[hoardr]{hoard}}
#' and deletes stored project information (not data)! If you provide
#' `project_names`, it will remove only those projects from the cache.
#' @details
#' The cache only stores information like project_name, token_name, directory
#' location, and other details from [setup_project()]. If you want to truly
#' delete the project files, you must do so at the project directory you set up.
#' @inheritParams sync
#' @examples
#' \dontrun{
#' cache_clear("OLD_PROJECT")
#' cache_clear() # every project
#' }
#' @family Cache Functions
#' @keywords Cache
#' @seealso \code{vignette("Cache", package = "REDCapSync")}
#' @returns Message of outcome and invisible NULL.
#' @export
cache_clear <- function(project_names = NULL) {
  cache <- get_cache()
  if (!is.null(project_names)) {
    for (project_name in project_names) {
      cache_remove_project(project_name)
    }
    return(invisible())
  }
  if (cache$exists("projects.rds")$exists) {
    cache$delete("projects.rds")
  }
  cli_alert_warning("You must delete files manually from the directory.")
  cli_alert_wrap(
    "REDCapSync cache cleared!",
    file = cache_path(),
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
  cache$mkdir()
  cache
}
#' @noRd
cache_path <- function() {
  cache <- get_cache()
  path <- sanitize_path(cache$cache_path_get())
  path
}
#' @noRd
sweep_dirs_for_cache <- function(project_names = NULL) {
  # for if others are using the same object
  projects <- get_projects()
  if (nrow(projects) == 0L) {
    return(invisible())
  }
  project_list <- split(projects, projects$project_name)
  had_change <- FALSE
  all_project_names <- names(project_list)
  if (is.null(project_names)) {
    project_names <- all_project_names
  }
  project_names <- project_names[which(project_names %in% all_project_names)]
  updated_projects <- NULL
  for (project_name in project_names) {
    from_cache <- project_list[[project_name]]
    expected_path <- get_project_path(
      project_name = project_name,
      dir_path = from_cache$dir_path,
      type = "details"
    )
    from_cache <- try_else_null(assert_project_details(from_cache, nrows = 1L))
    to_cache <- NULL
    if (file.exists(expected_path)) {
      to_cache <- try_else_null({
        x <- suppressWarnings({
          readRDS(expected_path)
        })
        assert_project_details(x, nrows = 1L)
        x
      })
    }
    if (is.null(from_cache) || is.null(to_cache)) {
      loaded_cache <- try_else_null({
        load_project(project_name = project_name)$.internal |>
          extract_project_details()
      })
      if (is.null(loaded_cache)) {
        danger_message <- paste0("Unable to load ",
                                 project_name,
                                 ". Removed! Retry `setup_project(...)`")
        cli_alert_danger(danger_message)
        project_list[[project_name]] <- NULL
        had_change <- TRUE
      } else {
        project_list[[project_name]] <- loaded_cache
      }
    }
    rownames(from_cache) <- NULL
    rownames(to_cache) <- NULL
    if (!is.null(to_cache) && !identical(from_cache, to_cache)) {
      if (!is.null(from_cache)) {
        to_cache$dir_path <- from_cache$dir_path
      } # could cause issue?
      project_list[[project_name]] <- to_cache
      updated_projects <-  append(updated_projects, project_name)
      had_change <- TRUE
    }
  }
  if (had_change) {
    updating_message <- paste0("Updated cache! This can happen when using",
                               " multiple computers or with version changes.")
    cli_alert_info(updating_message)
    save_projects_to_cache(bind_rows(project_list), silent = FALSE)
  }
}
