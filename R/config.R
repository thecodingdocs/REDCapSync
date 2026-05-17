#' @title Configuration
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Internal configuration helpers used to retrieve package configuration
#' values from options or environment variables.
#'
#' Configuration is resolved in the following order:
#' 1. `getOption("redcapsync.config.option.name")`
#' 2. `Sys.getenv("REDCAPSYNC_CONFIG_OPTION_NAME")` # skipped for functions
#' 3. Default if unable to find and validate from above.
#'
#' @details
#' The config function is operational but only some methods presently
#' affect internal code. Most users will not ever need to modify default config.
#' This is included to improve future versions of the package.
#' * Working configs: `allow.test.names`, `cache.dir`, `keyring`,
#' `keyring.service`, `openxlsx.header.style`, `openxlsx.body.style`.
#' * Partial coverage: `offline`.
#' * Placeholder: `verbose`, `show.api.messages`
#'
#' ## allow.test.names
#' Logical for [setup_project()] allowing `project_name` starting with TEST_.
#' Default is `FALSE`. This is provided for developers and testing environments.
#' ```
#' # check current value package is using...
#' config$allow.test.names()
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.allow.test.names") # get
#' options(redcapsync.config.allow.test.names = FALSE) # set
#'
#' # set with ennvar (which will be prioritized when options not defined)
#' Sys.getenv("REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES")  # get
#' Sys.setenv(REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES = FALSE) # or set in .Renviron
#' ```
#'
#' ## show.api.messages
#' Logical for showing display API messages from REDCapR and redcapAPI.
#' Default is `FALSE`.
#'
#' ```
#' # check current value package is using...
#' config$show.api.messages()
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.show.api.messages") # get
#' options(redcapsync.config.show.api.messages = FALSE) # set
#'
#' # set with ennvar (which will be prioritized when options not defined)
#' Sys.getenv("REDCAPSYNC_CONFIG_SHOW_API_MESSAGES") # get
#' Sys.setenv(REDCAPSYNC_CONFIG_SHOW_API_MESSAGES = FALSE) # or set in .Renviron
#' ```
#'
#' ## verbose
#' Logical for showing display API messages from REDCapR and redcapAPI.
#' Default is `FALSE`.
#'
#' ```
#' # check current value package is using...
#' config$verbose()
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.verbose") # get
#' options(redcapsync.config.verbose = FALSE) # set
#'
#' # set with ennvar (which will be prioritized when options not defined)
#' Sys.getenv("REDCAPSYNC_CONFIG_VERBOSE") # get
#' Sys.setenv(REDCAPSYNC_CONFIG_VERBOSE = FALSE) # or set in .Renviron
#' ```
#'
#' ## offline
#' Logical for offline, which if TRUE will block any API calls.
#' Default is `FALSE`.
#'
#' ```
#' # check current value package is using...
#' config$offline()
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.offline") # get
#' options(redcapsync.config.offline = FALSE) # set
#'
#' # set with ennvar (which will be prioritized when options not defined)
#' Sys.getenv("REDCAPSYNC_CONFIG_OFFLINE") # get
#' Sys.setenv(REDCAPSYNC_CONFIG_OFFLINE = FALSE) # or set in .Renviron
#' ```
#'
#' ## cache.dir
#' Character file path overriding the default cache directory. Default follow
#' system standards via rappdir, hoardr, or R_USER_CACHE_DIR
#'
#' ```
#' # check current value package is using...
#' config$cache.dir()
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.cache.dir") # get
#' options(redcapsync.config.cache.dir = "file/path/to/keep/cache") # set
#'
#' # set with ennvar (which will be prioritized when options not defined)
#' Sys.getenv("REDCAPSYNC_CONFIG_CACHE_DIR") # get
#' Sys.setenv(REDCAPSYNC_CONFIG_CACHE_DIR = "file/path/to/keep/cache") # set
#' ```
#'
#' ## keyring
#' Character keyring name (parameter from \code{\link[keyring]{keyring}}
#' package). Default is NULL, which is at the system level. For locking use a
#' keyring like "REDCapSync"
#'
#' ```
#' # check current value package is using...
#' config$keyring()
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.keyring") # get
#' options(redcapsync.config.keyring = "REDCapSync") # set
#'
#' # set with ennvar (which will be prioritized when options not defined)
#' Sys.getenv("REDCAPSYNC_CONFIG_KEYRING") # get
#' Sys.setenv(REDCAPSYNC_CONFIG_KEYRING = "REDCapSync") # set in session
#' ```
#'
#' ## keyring.service
#' Character keyring service name (parameter from \code{\link[keyring]{keyring}}
#' package). Default is "R-REDCapSync".
#'
#' ```
#' # check current value package is using...
#' config$keyring.service()
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.keyring.service") # get
#' options(redcapsync.config.keyring.service = "REDCapSync") # set
#'
#' # set with ennvar (which will be prioritized when options not defined)
#' Sys.getenv("REDCAPSYNC_CONFIG_KEYRING_SERVICE") # get
#' Sys.setenv(REDCAPSYNC_CONFIG_KEYRING_SERVICE = "REDCapSync") # set in session
#' ```
#'
#' ## openxlsx.header.style
#' Excel sheet header using [openxlsx::createStyle()]
#'
#' ```
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.openxlsx.header.style") # get
#' new_style <- openxlsx::createStyle(fontSize = 12L)
#' options(redcapsync.config.openxlsx.header.style = new_style)
#'# does not look up envvar due to it's function type
#' ```
#'
#' ## openxlsx.body.style
#' Excel sheet body using [openxlsx::createStyle()]
#'
#' ```
#' # set with options (which will be prioritized over envvar)
#' getOption("redcapsync.config.openxlsx.body.style") # get
#' new_style <- openxlsx::createStyle(fontSize = 12L)
#' options(redcapsync.config.openxlsx.body.style = new_style)
#'# does not look up envvar due to it's function type
#' ```
#'
#' # Option Names (searched first)
#'
#' ```
#' option_list <- list(
#'   redcapsync.config.allow.test.names = NULL,
#'   redcapsync.config.show.api.messages = NULL,
#'   redcapsync.config.verbose = NULL,
#'   redcapsync.config.offline = NULL,
#'   redcapsync.config.cache.dir = NULL,
#'   redcapsync.config.keyring = NULL,
#'   redcapsync.config.keyring.service = NULL,
#'   redcapsync.config.openxlsx.header.style = NULL,
#'   redcapsync.config.openxlsx.body.style = NULL
#' )
#' ```
#'
#' # Environment Variable Names (searched second)
#' ```
#' envvar_list <- list(
#'   REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES = NA,
#'   REDCAPSYNC_CONFIG_SHOW_API_MESSAGES = NA,
#'   REDCAPSYNC_CONFIG_VERBOSE = NA,
#'   REDCAPSYNC_CONFIG_OFFLINE = NA,
#'   R_USER_CACHE_DIR = NA, # affects your entire cache for any package
#'   REDCAPSYNC_CONFIG_CACHE_DIR = NA, # affects only REDCapSync Cache
#'   REDCAPSYNC_CONFIG_KEYRING = NA,
#'   REDCAPSYNC_CONFIG_KEYRING_SERVICE = NA
#' )
#' ```
#'
#' @seealso
#'
#' vignette("Projects", package = "REDCapSync")
#'
#' [setup_project] for initializing projects
#'
#' @examples
#'
#' # disable with environment variable
#' Sys.setenv(REDCAPSYNC_CONFIG_OFFLINE = FALSE)
#'
#' config$offline()
#'
#' # change to offline
#' options(redcapsync.config.offline = TRUE)
#'
#' config$offline()
#' @returns list of functions that returns config values
#' @export
config <- list(
  allow.test.names = function(default = FALSE) {
    config_get(opt_name = "allow.test.names",
               type = "logical",
               default = default)
  },
  show.api.messages = function(default = FALSE) {
    config_get(opt_name = "show.api.messages",
               type = "logical",
               default = default)
  },
  verbose = function(default = FALSE) {
    config_get(opt_name = "verbose",
               type = "logical",
               default = default)
  },
  offline = function(default = FALSE) {
    config_get(opt_name = "offline",
               type = "logical",
               default = default)
  },
  cache.dir = function(default = cache_path_default()) {
    config_get(opt_name = "cache.dir",
               type = "filepath",
               default = default)
  },
  keyring = function(default = NULL) {
    config_get(opt_name = "keyring",
               type = "character",
               default = default)
  },
  keyring.service = function(default = "R-REDCapSync") {
    config_get(opt_name = "keyring.service",
               type = "character",
               default = default)
  },
  openxlsx.header.style = function(default = openxlsx_header_style()) {
    config_get(opt_name = "openxlsx.header.style",
               type = "openxlsx_style",
               default = default)
  },
  openxlsx.body.style = function(default = openxlsx_body_style()) {
    config_get(opt_name = "openxlsx.body.style",
               type = "openxlsx_style",
               default = default)
  }
)
#' @noRd
config_get <- function(opt_name, type, default) {
  assert_choice(opt_name, names(config))
  assert_choice(type, OPTION_TYPES)
  # check options first
  opt_key <- paste("REDCapSync", "config", opt_name, sep = ".") |> tolower()
  opt_val <- getOption(opt_key)
  if (!is.null(opt_val)) {
    return(config_validate(opt_name, opt_val, type, default))
  }
  # check envvar second
  env_name <- chartr(".", "_", opt_name)
  env_key <- paste("REDCapSync", "config", env_name, sep = "_") |> toupper()
  env_val <- Sys.getenv(env_key, unset = NA)
  if (!is.na(env_val) && nzchar(env_val) && type != "openxlsx_style") {
    return(config_validate(opt_name, env_val, type, default))
  }
  default
}
#' @noRd
config_validate <- function(opt_name, value, type, default) {
  assert_choice(opt_name, names(config))
  assert_choice(type, OPTION_TYPES)
  # add warning messages
  if (identical(type, "logical")) {
    if (test_character(value, len = 1L)) {
      if (toupper(value) %in% c("TRUE", "FALSE")) {
        value <- value |> toupper() |> as.logical()
      }
    }
    if (test_logical(value, len = 1L)) {
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
      # meesage about not existing?
      return(value)
    }
  }
  if (identical(type, "character")) {
    if (test_character(value, min.chars = 1L, any.missing = FALSE, len = 1L)) {
      if (toupper(value) != "NULL") {
        return(value)
      }
    }
  }
  default
}
OPTION_TYPES <- c("logical", "filepath", "character", "openxlsx_style")
