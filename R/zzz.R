.onLoad <- function(libname, pkgname) {
  get_cache()
}
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    # packageStartupMessage(
    #   "REDCapSync Loaded! Check available projects with `get_projects()`"
    # )
  }
}
