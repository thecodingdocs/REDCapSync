#' @title config
#' @description
#' configuration
#'
#' @keywords internal
config <- list(
  allow.test.names = function(default = FALSE) {
    config_get(opt_name = "allow.test.names",
               type    = "logical",
               default = default)
  },
  show.api.messages = function(default = FALSE) {
    config_get(opt_name = "show.api.messages",
               type = "logical",
               default = default)
  },
  verbose = function(default = TRUE) {
    config_get(opt_name = "verbose",
               type = "logical",
               default = default)
  },
  cache.dir = function (default = cache_path_default()){
    config_get(opt_name = "cache.dir",
               type = "filepath",
               default = default)
  },
  header.style = function(default = .header_style) {
    config_get(opt_name = "header.style",
               type = "openxlsx_style",
               default = default)
  },
  body.style = function(default = .body_style) {
    config_get(opt_name = "body.style",
               type = "openxlsx_style",
               default = default)
  }
)
config_get <- function(opt_name, type, default) {
  assert_choice(opt_name, names(config))
  assert_choice(type, c("logical", "openxlsx_style", "filepath"))
  opt_key <- paste("REDCapSync", "config", opt_name, sep = ".") |> tolower()
  opt_val <- getOption(opt_key)
  if (!is.null(opt_val)) {
    return(config_validate(opt_name, opt_val, type, default))
  }
  env_key <- paste("REDCapSync", "config", chartr(".", "_", opt_name), sep = "_") |> toupper()
  env_val <- Sys.getenv(env_key, unset = NA)
  if (!is.na(env_val) && nzchar(env_val) && type != "openxlsx_style") {
    return(config_validate(opt_name, env_val, type, default))
  }
  default
}
config_validate <- function(opt_name, value, type, default) {
  if (identical(type, "logical")) {
    if (test_character(value, len = 1)) {
      if (toupper(value) %in% c("TRUE", "FALSE")) {
        value <- value |> toupper() |> as.logical()
      }
    }
    if (test_logical(value, len = 1)) {
      return(value)
    }
  }
  if (identical(type, "openxlsx_style")) {
    if (test_openxlsx_style(value)) {
      return(value)
    }
  }
  if (identical(type, "filepath")) {
    if (test_directory_exists(value)) {
      return(value)
    }
  }
  default
}
