#' @title REDCapSync Encapsulated Datsaset Object
#' @description
#' [R6][R6::R6Class] project object for [REDCapSync]
#' @param project a list object meant to be stored internally within R6
#' @param dataset_name dataset name setup with `project$add_dataset`
#' @examples
#' dataset <- load_project("TEST_CLASSIC")$generate_dataset("REDCapSync")
#'
#' @seealso
#' \link{setup_project} for initializing the `project` object.'
#' @returns
#' An R6ClassGenerator which is used internally to create or load a
#' dataset object for the user
#' @name dataset
#' @rdname dataset
#' @keywords internal
REDCapSyncDataset <- R6Class(
  "REDCapSyncDataset",
  active = list(
    #' @field project_name Read-only character string of project_name as
    #' assigned with [setup_project].
    project_name = function(value) {
      if (!missing(value)) {
        message(
          "`project_name` is read only. To change `setup_project()`"
        )
      }
      private$details$project_name
    },
    #' @field dataset_name Read-only character string of dataset_name
    dataset_name = function(value) {
      if (!missing(value)) {
        message(
          "`dataset_name` is read only."
        )
      }
      private$details$dataset_name
    },
    #' @field n_records Read-only integer of n_records
    n_records = function(value) {
      if (!missing(value)) {
        message(
          "`n_records` is read only."
        )
      }
      private$details$n_records
    }
  ),
  public = list(
    #' @description
    #' The end user will not see `dataset$new()`. This is handled internally.
    #' Users should construct objects using [REDCapSyncProject].
    initialize = function(project, dataset_name) {
      assert_setup_project(project)
      dataset_name <- "REDCapSync"
      dataset <- project |>
        generate_project_dataset(dataset_name = dataset_name,
                                 internal_use = TRUE)
      self$data <- dataset$data
      self$metadata <- dataset$metadata
      self$records <- dataset$records
      self$users <- dataset$users
      self$log <- dataset$log
      self$comments <- dataset$comments
      private$details <- list(
        project_name = project$project_name,
        dataset_name = dataset_name,
        n_records = project$datasets[[dataset_name]]$n_records,
        help = project$links$help
      )
      invisible(self)
    },
    #' @description Print some key dataset information
    print = function() {
      cli_h1("REDCapSyncProject")
      cli_text("Project: {private$details$project_name}")
      cli_text("Dataset: {private$details$dataset_name}")
      cli_h2("Help")
      cli_text("Help: {.url {private$details$help}}") # getting started
      cli_text("Datasets: {.vignette REDCapSync::Datasets}")
      cli_text("Tokens: {.vignette REDCapSync::Tokens}")
      invisible(self)
    },
    #' @field data list of data where names are forms
    data = NULL,
    #' @field metadata list of metadata
    metadata = NULL,
    #' @field records data.frame of records with timestamps
    records = NULL,
    #' @field users data.frame of users with timestamps
    users = NULL,
    #' @field log data.frame of log
    log = NULL,
    #' @field comments data.frame of comments
    comments = NULL
  ),
  private = list(
    details = NULL
  ),
  lock_objects = FALSE,
  cloneable = FALSE
)
