#' @noRd
.onLoad <- function(libname, pkgname) {
  user_options <- options()
  default_pkg_options <- list(
    REDCapSync.header_style = .header_style,
    REDCapSync.body_style = .body_style,
    REDCapSync.show_REDCapR_messages = FALSE,
    REDCapSync.verbose = TRUE
  )
  to_set <- !(names(default_pkg_options) %in% names(user_options))
  if (any(to_set)) {
    options(default_pkg_options[to_set])
  }
  get_cache()
  invisible()
}
