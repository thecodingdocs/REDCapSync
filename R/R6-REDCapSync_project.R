#' @title Setup or Load REDCapSync Project
#' @description
#' [R6][R6::R6Class] project object for [REDCapSync]
#' Main class for managing REDCap data, metadata, and sync operations.
#' Users should construct objects using [setup_project()].
#' @return An R6ClassGenerator
#' @keywords internal
REDCapSync_project <- R6Class(
  "REDCapSync_project",
  public = list(
    #' @description
    #' The end user will not see `project$new()`. This is handled internally.
    #' Users should construct objects using [setup_project()]. The remain methods
    #' will be accessible to any user.
    #' @param project a list object meant to be stored internally within R6
    initialize = function(project) {
      assert_setup_project(project)
      private$project <- project
      invisible()
    },
    #' @description Print project metadata
    info = function(){
      message("short_name: ",private$project$short_name)
      message("directory: ",private$project$dir_path)
    },
    #' @description  Add a new summary entry
    add_summary = function(){
      add_project_summary()
      invisible(self)
    },
    #' @description  Add a new summary entry
    #' @param summary_name Character of summary_name.
    generate_summary = function(summary_name){
      private$project <- private$project %>% generate_project_summary(
        summary_name = summary_name
        )
    },
    #' @description  Add a new summary entry
    add_field = function(){
      message("Added field!")
      invisible(self)
    },
    #' @description
    #' Updates the REDCap database (`project` object) by fetching the latest data
    #' from the REDCap server.
    #'
    #' @details
    #' This function updates the REDCap database by fetching the latest data from
    #' the REDCap server. It supports various options such as forcing a fresh
    #' update, checking logs for a specified number of days, and retrieving files
    #' from REDCap. The function can also handle metadata-only updates and batch
    #' processing.
    #'
    #' @param summarize Logical (TRUE/FALSE). If TRUE, summarizes data to directory.
    #' @param save_to_dir Logical (TRUE/FALSE). If TRUE, saves the updated data to
    #' the directory. Default is `TRUE`.
    #' @param hard_check Will check REDCap even if not due (see `sync_frequency`
    #' parameter from `setup_project()`)
    #' @param hard_reset Logical that forces a fresh update if TRUE. Default is
    #' `FALSE`.
    #' @return Messages for confirmation.
    #' @seealso
    #' \link{setup_project} for initializing the `project` object.'
    sync = function(summarize = TRUE,
                    save_to_dir = TRUE,
                    hard_check = FALSE,
                    hard_reset = FALSE){
      private$project <- sync_project(
        project = private$project,
        summarize = summarize,
        save_to_dir = save_to_dir,
        hard_check = hard_check,
        hard_reset = hard_reset)
      invisible(self)
    },
    #' @description  Add a new summary entry
    save = function(){
      save_project(private$project)
      invisible(self)
    },
    #' @description  Returns list of data or the specified form.
    #' @param type string of either "fields","forms", or "choices"
    show_metadata = function(type = "fields"){
      if(is.null(type)){
        return(private$project$metadata)
      }
      private$project$metadata[[type]]
    },
    #' @description  Returns list of data or the specified form.
    #' @param form string of raw form name such as "survey_one"
    show_data = function(form) {
      if(missing(form)){
        return(private$project$data)
      }
      private$project$data[[form]]
    },
    #' @description  returns internal list
    use = function(){
      invisible(private$project)
    }
  ),
  private = list(
    project = NULL
  ),
  cloneable = FALSE
)
