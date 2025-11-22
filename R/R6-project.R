#' @title Setup or Load REDCapSync Project
#' @description
#' [R6][R6::R6Class] project object for [REDCapSync]
#' Main class for managing REDCap data, metadata, and sync operations.
#' Users should construct objects using [setup_project()].
#' @return A `project` R6 object.
#' @keywords internal
project <- R6::R6Class(
  "project",
  public = list(
    #' @description
    #' The end user will not see `project$new()`. This is handled internally.
    #' Users should construct objects using [setup_project()]. The remain methods
    #' will be accessible to any user.
    #' @param short_name Character project identifier.
    initialize = function(short_name) {
      if(missing(short_name)){
      }
      private$load(short_name)
    },
    #' @description Print project metadata
    info = function(){
      message("short_name: ",private$project$short_name)
      message("directory: ",private$project$dir_path)
    },
    #' @description  Add a new summary entry
    add_summary = function(){
      add_project_summary()
    },
    #' @description  Add a new summary entry
    #' @param short_name Character project identifier.
    generate_summary = function(short_name){
      private %>% generate_summary(short_name)
    },
    #' @description  Add a new summary entry
    add_field = function(){
      message("Added field!")
    },
    #' @description  Add a new summary entry
    sync = function(){
      private$project <- sync_project(private$project,summarize = FALSE)
    },
    #' @description  Add a new summary entry
    save = function(){
      save_project(private$project)
    },
    #' @description  returns internal list
    use = function(){
      invisible(private$project)
    }
  ),
  private = list(
    project = NULL,
    load = function(short_name) {
      private$project <- load_project(short_name)
    }
  ),
  cloneable = FALSE
)
#' @title Setup or Load REDCapSync Project
#' @description
#' Setup or Load the `project` object for pipeline. Once it is setup see
#' [project] for the methods
#'
#' @details
#' This function sets up the `project` object by storing the REDCap API token
#' and
#' other configurations required for interacting with the REDCap server.
#' It ensures that the token is valid and ready for use in subsequent API calls.
#' Neither function directly attempts communication with REDCap.
#'
#' `setup_project` is used the first time you initialize/link a REDCap project.
#' Mainly, it sets your unique `short_name` and your intended directory.
#' Unless you run \code{hard_reset = TRUE} the default will first try load_project.
#' dir_path is technically optional but without it the user cannot
#' save/load/update projects.
#'
#' `load_project` can be used with just the `short_name` parameter after you
#' have
#' already run `setup_project` in the past with an established directory.
#' `dir_path`
#' is optional for this function but can be used if you relocated the directory.
#' @inheritParams setup_project
#' @inheritParams REDCapR::redcap_read
#' @return REDCapSync `project` list object.
#' @seealso
#' \code{\link[REDCapSync]{get_projects}} for retrieving a list of projects from
#' the directory cache.
#' @examplesIf FALSE
#' # Initialize the project object with the REDCap API token and URL
#' project <- setup_project(
#'   short_name = "TEST",
#'   dir_path = "path/to/secure/file/storage",
#'   redcap_uri = "https://redcap.yourinstitution.edu/api/"
#' )
#' project <- load_project("TEST")
#' @export
setup_r6_test <- function(short_name,
                          dir_path,
                          redcap_uri,
                          token_name = paste0("REDCapSync_", short_name),
                          sync_frequency = "daily",
                          labelled = TRUE,
                          hard_reset = FALSE,
                          records = NULL,
                          fields = NULL,
                          forms = NULL,
                          events = NULL,
                          filter_logic = NULL,
                          get_type = "identified",
                          metadata_only = FALSE,
                          batch_size_download = 2000,
                          batch_size_upload = 500,
                          entire_log = FALSE,
                          days_of_log = 10,
                          get_files = FALSE,
                          get_file_repository = FALSE,
                          original_file_names = FALSE,
                          add_default_fields = FALSE,
                          add_default_transformation = FALSE,
                          add_default_summaries = TRUE,
                          use_csv = FALSE,
                          silent = FALSE){
  projects <- get_projects()
  project$new(short_name)
}
