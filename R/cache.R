#' @title Get your Get Cache Path
#' @description
#' Included for transparency and confirmation/testing. This is where the basic
#' information about your projects is cached when you use the REDCapSync
#' package.
#' @details
#' This function checks the location of the cache established by
#' \code{\link[hoardr]{hoard}}.
#' \emph{No project data is stored here. Tokens are not stored here either.}
#' Key information stored here is `short_name` (primary key for REDCapSync
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
  cli_alert_warning(
    "You must delete any/all other files manually from the directory."
  )
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
  if (cache_exists()) {
    outcome <- cache_path() %>%
      file.path("projects.rds") %>%
      file.exists()
  } else {
    cli_alert_warning("Cache doesn't exist")
    outcome <- FALSE
  }
  outcome
}
#' @noRd
get_cache <- function() {
  cache <- hoardr::hoard()
  cache$cache_path_set(path = "REDCapSync", type = "user_cache_dir")
  cache$mkdir()
  cache
}
