#' @noRd
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.REDCapSync <- list(REDCapSync.header_style = .header_style,
                        REDCapSync.body_style = .body_style,
                        REDCapSync.show_REDCapR_messages = FALSE,
                        REDCapSync.verbose = TRUE)
  toset <- !(names(op.REDCapSync) %in% names(op))
  if (any(toset)) {
    options(op.REDCapSync[toset])
  }
  get_cache()
  invisible()
}
