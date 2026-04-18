#' @title Standardized Dataset from REDCap Project
#' @description
#' `r lifecycle::badge("experimental")`
#' [R6][R6::R6Class] project object for [REDCapSync]
#' @param project object from [setup_project()] or [load_project()].
#' @param dataset_name Character. The name of the configured dataset from which
#' to generate the dataset. *If you provide `dataset_name` all other parameters
#' are inherited according to what was set with `add_dataset`.
#' @param transformation_type Character scalar. How to transform data for the
#' dataset. Default is "default". Other options are "none", "flat",
#' "merge_non_repeating". "default" first merges non-repeating and if there are
#' repeating forms, it merges non-repeating variables to the right of repeating
#' instruments. "flat" is one-record, one-row, even if there are repeating
#' forms. "none" does not transform anything. "merge_non_repeating" still merges
#' all non-repeating instruments but does not merge them to repeating
#' instruments.
#' @param merge_form_name A character string representing the name of the merged
#' form. Default is "merged".
#' @param filter_field Character. The name of the field in the database to
#' filter on. Used with `filter_choices`.
#' @param filter_choices Vector. The values of `filter_field` used to define the
#' dataset. An alternative to providing a full `filter_list`.
#' @param filter_list Vector. The values of `filter_field` used to define the
#' dataset. Names are field names; values are the allowed value set(s). Use
#' either `filter_list` or `filter_field` with `filter_choices`.
#' @param filter_strict Logical. If `TRUE`, all forms will be filtered by
#' criteria. If `FALSE`, will convert original filter to ID column and filter
#' all other forms by that record. Default is `TRUE`.
#' @param form_names Character vector. Names of forms to include in the dataset.
#' Default is `NULL`, which includes all forms.
#' @param field_names Character vector. Names of specific fields to include in
#' the dataset. Default is `NULL`, which includes all fields.
#' @param exclude_identifiers Logical. Whether to exclude identifiers in the
#' data in the dataset. Default is `TRUE`.
#' @param exclude_free_text Logical. If `TRUE`, exclude free text fields
#' intended for deidentification workflows. Default is `FALSE`.
#' @param date_handling character string. One of `none`,`exclude_dates`,
#' `random_shift_by_record`, `random_shift_by_project`, `zero_by_record`, or
#' `zero_by_project`. Random shift is +/- 90 unless changed with options.
#' @param labelled Logical. If `TRUE`, the data will be converted to labelled.
#' If `FALSE`, returns raw coded values. Default is `TRUE`.
#' @param clean Logical. If `TRUE`, the data will be cleaned (e.g.,
#' standardizing missing/blank values) before summarizing. Default is `TRUE`. If
#'  missing codes are present in a number or date variable, R will convert
#'  missing codes to NA and will make that variable not upload compatible.
#' @param drop_blanks Logical. If `TRUE`, records with blank fields will be
#' dropped during cleaning. Default is `TRUE`.
#' @param drop_missing_codes Logical. If `TRUE`, will convert missing codes
#' to NA. Default is `FALSE`.
#' @param drop_others Character vector of other values that should be dropped.
#' @param include_metadata Logical. If `TRUE`, metadata will be included in the
#' dataset. Default is `TRUE`.
#' @param include_users Logical. If `TRUE`, user-related information will be
#' included in the dataset. Default is `TRUE`.
#' @param include_records Logical. If `TRUE`, a record dataset will be
#' included in the generated dataset. Default is `TRUE`.
#' @param include_log Logical. If `TRUE`, the log of changes will be included in
#' the dataset. Default is `TRUE`.
#' @param annotate_from_log Logical. If `TRUE`, the metadata, users, and records
#' will be annotated using the log. Default is `TRUE`.
#' @param include_comments Logical. If `TRUE`, the comments will be included.
#' Default is `TRUE`.
#' @param with_links Optional logical (TRUE/FALSE) for including links in Excel
#' sheets. Default is `FALSE`.
#' @param separate Optional logical (TRUE/FALSE) separating each form into
#' separate files as opposed to multi-tab Excel. Default is `FALSE`.
#' @param use_csv Logical (TRUE/FALSE). If TRUE, uses CSV files for data
#' storage. Default is `FALSE`
#' @param dir_other Character. The directory where the dataset file will be
#' saved. Default is the `output` folder within the database directory.
#' @param file_name Character. The base name of the file where the dataset will
#' be saved. Default is `<project_name>_<dataset_name>`.
#' @param envir environment variable such as [globalenv()]
#' @examples
#' save_dir <- tempdir()
#' dataset <- load_project("TEST_CLASSIC")$generate_dataset("REDCapSync")
#' # add quick custom variable
#' dataset$data$merged$letter_b <- dataset$data$merged$var_text_letters == "b"
#' # save data in custom location
#' dataset$save(dir_other = save_dir)
#'
#' @seealso
#' \code{vignette("Datasets", package = "REDCapSync")}
#' \link{setup_project} for initializing the `project` object.'
#' @returns
#' An R6ClassGenerator which is used internally to create or load a
#' dataset object for the user
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
      data_list <- save_project_data_list(self,
                                          with_links = with_links,
                                          separate = separate,
                                          use_csv = use_csv,
                                          dir_other = dir_other,
                                          file_name = file_name)
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
