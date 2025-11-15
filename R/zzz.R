.onLoad <- function(libname, pkgname) {
  # op <- options()
  # op.REDCapSync <- list(
  #   REDCapSync.sync_frequency = "daily"
  # )
  # toset <- !(names(op.REDCapSync) %in% names(op))
  # if (any(toset)) options(op.REDCapSync[toset])
  get_cache()
  invisible()
}
