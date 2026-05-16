#' @title REDCapSync Dataset Object
#' @description
#' `r lifecycle::badge("experimental")`
#' [R6][R6::R6Class] object representing a standardized dataset generated from
#' a [REDCapSync] project. Use this object to inspect transformed REDCap data,
#' export datasets, and assign prepared analysis tables into the calling
#' environment.
#'
#' @details
#' A `REDCapSyncDataset` can be created ad-hoc from a project with
#' `project$generate_dataset()`. For resusability, you can also define with
#' `project$add_dataset()` and then load with `project$load_dataset()`.
#'
#' ## Typical workflow
#' ```r
#' setup project from test object
#' project <- setup_project(
#'   project_name = "TEST_CLASSIC",
#'   dir_path = tempdir()
#' )
#' # Create and save a filtered dataset
#' dataset <- project$add_dataset(
#'   dataset_name = "analysis_set",
#'   filter_field = "var_yesno",
#'   filter_choices = "Yes",
#'   field_names = c("record_id", "ecog_at_diagnosis", "stage_at_diagnosis")
#'  )
#' # generate dataset for R environment
#' dataset <- project$load_dataset("analysis_set")
#' # optional send to global environment
#' dataset$to_envir(globalenv()) # keep in mind potential name conflicts
#' # Optional modify (final save depends on what is in the dataset object)
#' dataset$data$merged$stage_2 <- dataset$data$merged$stage_at_diagnosis == "II"
#' # save to directory
#' dataset$save() # can specify `dir_other`, by default saves to output folder
#' ```
#'
#' This object is designed for users who want a stable dataset output from
#' REDCap without modifying the underlying project. This is also used
#' behind-the-scenes in the RosyREDCap shiny app.
#'
#' ## Key features
#' - Stores a project-specific dataset definition and resulting data
#' - Keeps metadata, record details, user information, and comments in sync
#' - Saves datasets in Excel or CSV formats with optional hyperlinks
#' - Exports dataset components to a user-specified environment
#'
#' @param project Project object from [setup_project()] or [load_project()].
#' @param dataset_name Character. Name of the dataset to generate or load.
#'   If the dataset already exists in the project, the existing definition is
#'   reused.
#' @param transformation_type Character. Data transformation strategy: "default"
#'   (preferred merged output), "none" (raw data structure), or
#'   "merge_non_repeating" (merge only non-repeating forms). Default is
#'   "default".
#' @param merge_form_name Character. Name used for merged non-repeating records.
#'   Default is "merged".
#' @param filter_field Character. Field used for filtering the dataset.
#' @param filter_choices Vector. Allowed values for `filter_field`.
#' @param filter_list List. Named list mapping field names to allowed values.
#'   Use instead of `filter_field`/`filter_choices` for more complex filters.
#' @param filter_strict Logical. If `TRUE`, filters are applied to every form.
#'   If `FALSE`, filters apply only to the record identifier. Default is
#'   `TRUE`.
#' @param form_names Character vector. Forms to include in the dataset.
#'   Default is `NULL` (all forms).
#' @param field_names Character vector. Variables to include in the dataset.
#'   Default is `NULL` (all fields).
#' @param exclude_identifiers Logical. Remove identifier fields. Default is
#'   `TRUE`.
#' @param exclude_free_text Logical. Remove free text fields. Default is
#'   `FALSE`.
#' @param date_handling Character. Date handling method: "none",
#'   "exclude_dates", "random_shift_by_record", "random_shift_by_project",
#'   "zero_by_record", or "zero_by_project". Default is "none".
#' @param labelled Logical. Convert values to labelled vectors if `TRUE`.
#'   Default is `TRUE`.
#' @param clean Logical. Clean the dataset by standardizing missing values and
#'   blanks. Default is `TRUE`.
#' @param drop_blanks Logical. Drop records with blank fields. Default is
#'   `FALSE`.
#' @param drop_missing_codes Logical. Convert REDCap missing codes to `NA`.
#'   Default is `FALSE`.
#' @param drop_others Character vector of additional values to remove.
#' @param include_metadata Logical. Include field metadata in the dataset.
#'   Default is `TRUE`.
#' @param include_users Logical. Include user information in the dataset.
#'   Default is `TRUE`.
#' @param include_records Logical. Include record-level details. Default is
#'   `TRUE`.
#' @param include_log Logical. Include REDCap activity log details. Default is
#'   `FALSE`.
#' @param annotate_from_log Logical. Annotate metadata and records using the
#'   change log. Default is `TRUE`.
#' @param include_comments Logical. Include REDCap comments. Default is
#'   `FALSE`.
#' @param with_links Logical. Include hyperlinks in Excel exports. Default is
#'   `TRUE`.
#' @param separate Logical. Save each form as a separate file instead of a
#'   multi-sheet workbook. Default is `FALSE`.
#' @param use_csv Logical. Write CSV files instead of Excel. Default is
#'   `FALSE`.
#' @param dir_other Character. Directory where the dataset file should be
#'   saved. Defaults to the project's output folder.
#' @param file_name Character. Base file name for saved datasets. Defaults to
#'   `<project_name>_<dataset_name>`.
#' @param envir Environment to assign exported dataset objects. Default is
#'   `NULL`.
#' @examples
#' project <- load_project("TEST_CLASSIC")
#'
#' dataset <- project$generate_dataset(
#'   dataset_name = "stage_2_patients",
#'   filter_field = "stage_at_diagnosis",
#'   filter_choices = "II",
#'   field_names = c("record_id", "ecog_at_diagnosis", "stage_at_diagnosis")
#'  )
#'
#' dataset$save(dir_other = tempdir())
#'
#' @seealso
#' vignette("Datasets", package = "REDCapSync")
#' [setup_project] for initializing projects
#' [load_project] for loading existing projects
#' @returns
#' An R6 `REDCapSyncDataset` object containing dataset output, metadata,
#' records, user information, and optional REDCap log data.
#' @name dataset
#' @rdname dataset
#' @export
REDCapSyncDataset <- R6Class(
  "REDCapSyncDataset",
  active = list(
    #' @field project_details Read-only list of dataset details from
    #' [setup_project()].
    project_details = function(value) {
      if (!missing(value)) {
        message(
          "`project_details` is read only. To change use `setup_project()`"
        )
      }
      private$details$project_details
    },
    #' @field dataset_details Read-only list of dataset details
    dataset_details = function(value) {
      if (!missing(value)) {
        message(
          "`dataset_details` is read only. To change use `generate_dataset()`"
        )
      }
      private$details$dataset_details
    },
    #' @field redcap Read-only list of dataset details from [project]
    redcap = function(value) {
      if (!missing(value)) {
        message(
          "`redcap` is read only. To change use `generate_dataset()`"
        )
      }
      private$redcap_list
    },
    #' @field links Read-only list of dataset details from [project]
    links = function(value) {
      if (!missing(value)) {
        message(
          "`links` is read only. To change use `generate_dataset()`"
        )
      }
      private$links_list
    }
  ),
  public = list(
    #' @description
    #' The end user will not see `dataset$new()`. This is handled internally.
    #' Users should construct objects using [REDCapSyncProject].
    initialize = function(project,
                          dataset_name,
                          transformation_type = "default",
                          merge_form_name = "merged",
                          filter_field = NULL,
                          filter_choices = NULL,
                          filter_list = NULL,
                          filter_strict = TRUE,
                          field_names = NULL,
                          form_names = NULL,
                          exclude_identifiers = TRUE,
                          exclude_free_text = FALSE,
                          date_handling = "none",
                          labelled = TRUE,
                          clean = TRUE,
                          drop_blanks = FALSE,
                          drop_missing_codes = FALSE,
                          drop_others = NULL,
                          include_metadata = TRUE,
                          include_users = TRUE,
                          include_records = TRUE,
                          include_log = FALSE,
                          annotate_from_log = TRUE,
                          include_comments = FALSE) {
      assert_setup_project(project) # message about internal?
      dataset <- NULL
      if (dataset_name %in% names(project$datasets)) {
        dataset <- load_project_dataset(project = project,
                                        dataset_name = dataset_name)
      }
      if (is.null(dataset)) {
        dataset <- generate_project_dataset(
          project = project,
          dataset_name = dataset_name,
          transformation_type = transformation_type,
          merge_form_name = merge_form_name,
          filter_field = filter_field,
          filter_choices = filter_choices,
          filter_list = filter_list,
          filter_strict = filter_strict,
          field_names = field_names,
          form_names = form_names,
          exclude_identifiers = exclude_identifiers,
          exclude_free_text = exclude_free_text,
          date_handling = date_handling,
          labelled = labelled,
          clean = clean,
          drop_blanks = drop_blanks,
          drop_missing_codes = drop_missing_codes,
          drop_others = drop_others,
          include_metadata = include_metadata,
          include_users = include_users,
          include_records = include_records,
          include_log = include_log,
          annotate_from_log = annotate_from_log,
          include_comments = include_comments
        )
      }
      self$data <- dataset$data
      self$metadata <- dataset$metadata
      self$records <- dataset$records
      self$users <- dataset$users
      self$log <- dataset$log
      self$comments <- dataset$comments
      private$details$dataset_details <- dataset$dataset_details
      private$details$project_details <- dataset$project_details
      private$redcap_list <- project$redcap
      private$redcap_list$log <- NULL
      private$redcap_list$users <- NULL
      private$links_list <- project$links
      invisible(self)
    },
    #' @description Print some key dataset information
    print = function() {
      cli_h1("REDCapSyncDataset")
      cli_text("Project: {private$details$project_details$project_name}")
      cli_text("Dataset: {private$details$dataset_details$dataset_name}")
      help_cli_text()
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
    comments = NULL,
    #' @description Return flat list
    save = function(with_links = TRUE,
                    separate = FALSE,
                    use_csv = FALSE,
                    dir_other = NULL,
                    file_name = NULL) {
      data_list <- save_project_data_list(
        data_list = self,
        with_links = with_links,
        separate = separate,
        use_csv = use_csv,
        dir_other = dir_other,
        file_name = file_name
      )
      # would be nice if updated supclass project in future version
      invisible(data_list)
    },
    #' @description export dataset to envir of your choosing. Keep in mind
    #' potential name conflicts
    to_envir = function(envir = NULL) {
      data_list <- data_list_to_save(self)
      if (!is.null(envir)) {
        list2env(data_list, envir = envir)
      }
      invisible(data_list)
    }
  ),
  private = list(
    details = NULL,
    redcap_list = NULL,
    links_list = NULL
  ),
  lock_objects = FALSE,
  cloneable = FALSE
)
