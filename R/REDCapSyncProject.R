#' @title REDCapSync Project Object
#' @description
#' [R6][R6::R6Class] project object for managing REDCap data access and
#' synchronization. The project object is your main interface to REDCap data
#' through the [REDCapSync] package. It stores your REDCap configuration, data,
#' metadata, and provides methods to sync, transform, and export your data.
#'
#' @details
#' ## Workflow
#'
#' **Initialize** a project with [setup_project()]:
#' ```r
#' project <- setup_project(
#'   project_name = "FIRST_PROJECT",
#'   redcap_uri = "https://redcap.yourinstitution.edu/api/",
#'   dir_path = "~/redcap_projects"
#' )
#' ```
#'
#' **Synchronize** REDCap data into your project object using the REDCap API
#' and log-based change detection:
#' ```r
#' project$sync()
#' ```
#'
#' **Load** previously saved REDCap project object using cache:
#' ```r
#' project <- load_project("FIRST_PROJECT)
#' projects$names() # should see "FIRST_PROJECT"
#' ```
#'
#' **Access** REDCap data, metadata, and users via read-only fields:
#' ```r
#' project$data           # Named list of REDCap forms/instruments
#' project$metadata       # REDCap field definitions, forms, and choices
#' project$redcap$users   # REDCap user information
#' project$redcap$log     # REDCap log information
#'
#' # you can bypass read-only if needed
#' users <- project$redcap$users
#' ```
#'
#' **Transform and export** your data into clean, analysis-ready datasets to be
#' used in R and/or Excel:
#' ```r
#' project$add_dataset("analysis_data", ...) # adds to project for re-use
#' project$save_datasets() # can save to Excel but is also part of future syncs
#' ```
#'
#' **Upload** corrected or new data back to REDCap:
#' ```r
#' project$upload(updated_data)
#' ```
#'
#' ## Design Features
#'
#' The project object uses **log-based sync**: Since REDCap maintains a detailed
#' change log, `project$sync()` only retrieves and updates records that have
#' changed since the last sync. This dramatically reduces API calls and improves
#' performance for large projects.
#'
#' All methods use **method chaining**: Methods invisibly return the project
#' object (self), allowing fluent code:
#' ```r
#' dataset <- load_project("TEST_CLASSIC")$sync()$load_dataset("REDCapSync")
#' ```
#'
#' ## Read-Only Fields
#'
#' - `project$project_name`: Project identifier (set with [setup_project])
#' - `project$dir_path`: Persistent storage directory (set with [setup_project])
#' - `project$data`: Named list of synchronized REDCap forms
#' - `project$metadata`: REDCap field metadata (forms, fields, choices)
#' - `project$redcap`: REDCap project info, users, and activity log
#' - `project$.internal`: Internal project object (for advanced use)
#'
#' @param dataset_name Character. Name of the dataset configuration to create,
#' load, or reference.
#' @param transformation_type Character. How to transform data: "default"
#' (merge non-repeating then add to repeating), "none" (no transformation), or
#' "merge_non_repeating" (merge non-repeating only). Default is "default".
#' @param merge_form_name Character. Name for the merged non-repeating form.
#' Default is "merged".
#' @param filter_field Character. Field name to filter dataset on.
#' @param filter_choices Vector. Allowed values for `filter_field`.
#' @param filter_list List. Named list where names are field names and values
#' are allowed value sets. Alternative to `filter_field`/`filter_choices`.
#' @param filter_strict Logical. If `TRUE`, filter all forms. If `FALSE`, apply
#' filter only to records (ID column). Default is `TRUE`.
#' @param form_names Character vector. Forms to include. Default `NULL` includes
#' all forms.
#' @param field_names Character vector. Fields to include. Default `NULL`
#' includes all fields.
#' @param exclude_identifiers Logical. Exclude identifier fields. Default is
#' `TRUE`.
#' @param exclude_free_text Logical. Exclude free-text fields (for
#' deidentification). Default is `FALSE`.
#' @param date_handling Character. Date handling strategy: "none",
#' "exclude_dates", "random_shift_by_record", "random_shift_by_project",
#' "zero_by_record", or "zero_by_project". Default is "none".
#' @param labelled Logical. Convert to labelled data if `TRUE`. Default is
#' `TRUE`.
#' @param clean Logical. Clean data (standardize missing values). Default is
#' `TRUE`.
#' @param drop_blanks Logical. Drop records with blank fields. Default is
#' `TRUE`.
#' @param drop_missing_codes Logical. Convert REDCap missing codes to `NA`.
#' Default is `FALSE`.
#' @param drop_others Character vector of additional values to drop.
#' @param include_metadata Logical. Include field metadata in dataset. Default
#' is `TRUE`.
#' @param include_users Logical. Include user information. Default is `TRUE`.
#' @param include_records Logical. Include record-level information. Default is
#' `TRUE`.
#' @param include_log Logical. Include REDCap activity log. Default is `FALSE`.
#' @param annotate_from_log Logical. Annotate data using the REDCap log. Default
#' is `TRUE`.
#' @param include_comments Logical. Include field comments. Default is `TRUE`.
#' @param hard_reset Logical. Overwrite existing dataset files. Default is
#' `FALSE`.
#' @param with_links Logical. Include hyperlinks in Excel exports. Default is
#' `FALSE`.
#' @param separate Logical. Separate each form into distinct files (vs.
#' multi-tab Excel). Default is `FALSE`.
#' @param use_csv Logical. Use CSV format instead of Excel. Default is `FALSE`.
#' @param dir_other Character. Output directory (default is project's `output`
#' folder).
#' @param file_name Character. Dataset file name (default is
#' `<project_name>_<dataset_name>`).
#' @param envir Environment to assign dataset objects. Default is `NULL`.
#' @param form Character. REDCap form/instrument name, e.g., "survey_one".
#' @param link_type Character. REDCap link type: "base", "home", "record_home",
#' "records_dashboard", "api", "api_playground", "codebook", "user_rights",
#' "setup", "logging", "designer", "dictionary", "data_quality", or
#' "identifiers".
#' @param open_browser Logical. Open link in browser. Default is `TRUE`.
#' @param record Character. Record ID.
#' @param page Character. REDCap form/instrument name.
#' @param instance Character. Repeating instance number.
#' @param save_to_dir Logical. Save updated project object to directory. Default
#' is `TRUE`.
#' @param save_datasets Logical. Save dataset outputs during sync. Default is
#' `TRUE`.
#' @param hard_check Logical. Force REDCap API check regardless of
#' `sync_frequency`. Default is `FALSE`.
#' @param type Character. Metadata type to retrieve: "fields", "forms", or
#' "choices".
#' @param dataset_names Character vector. Dataset names to remove/operate on.
#' Default is `NULL`.
#' @param to_be_uploaded Data frame in raw coded format ready to upload to
#' REDCap.
#'
#' @examples
#' # Load a test project
#' project <- setup_project("TEST_CLASSIC", dir_path = tempdir())
#'
#' # Sync data from REDCap
#' project$sync()
#'
#' # Access data and metadata
#' head(project$data$text)
#' project$metadata$fields[1:5, ]
#'
#' # Create and save a filtered dataset
#' dataset <- project$add_dataset(
#'   dataset_name = "analysis_set",
#'   filter_field = "var_yesno",
#'   filter_choices = "Yes",
#'   field_names = c("record_id", "ecog_at_diagnosis", "stage_at_diagnosis")
#'  )
#' # generate dataset for R environment
#' dataset <- project$generate_dataset("analysis_set")
#' # Optional modify
#' dataset$data$merged$stage_2 <- dataset$data$merged$stage_at_diagnosis == "II"
#' # save to directory
#' dataset$save()
#'
#' @seealso
#' [setup_project] for initializing a new project
#' [load_project] for loading an existing project
#'
#' @returns
#' An R6 `REDCapSyncProject` class generator for internal use. Users interact
#' with instances created by [setup_project] or [load_project].
#' @name project
#' @rdname project
#' @export
REDCapSyncProject <- R6Class(
  "REDCapSyncProject",
  #' @description
  #' Active binding are read-only
  active = list(
    #' @field project_name Read-only character string of project_name as
    #' assigned with [setup_project].
    project_name = function(value) {
      if (!missing(value)) {
        message(
          "`project_name` is read only. To change `setup_project()`"
        )
      }
      private$project$project_name
    },
    #' @field dir_path Read-only directory path assigned with [setup_project].
    dir_path = function(value) {
      if (!missing(value)) {
        message(
          "`dir_path` is read only. To change `setup_project()`"
        )
      }
      private$project$dir_path
    },
    #' @field data Read-only named list where each name is an instrument name.
    #' See public methods for [REDCapSyncProject].
    data = function(value) {
      if (!missing(value)) {
        cli_alert_danger(
          paste0(
            "`data` is read only. To change REDCap data either use",
            "`project$upload()` or work with an output by assigning the data",
            "with `form_to_edit <-project$data$<form_name>`. Alternatively",
            "use the output from `project$generate_dataset()`"
          )
        )
      }
      private$project$data
    },
    #' @field metadata Read-only named list with REDCap metadata. See
    #' public methods for [REDCapSyncProject].
    metadata = function(value) {
      if (!missing(value)) {
        cli_alert_danger(
          paste(
            "`metadata` is read only. To change, do so on the",
            "REDCap website, or use the REDCap API. If you just want to work",
            "with the object more in R, reassign the object like,",
            "`fields <- project$metadata$fields`. Alternatively use the output",
            "from `project$generate_dataset()`. See REDCap Designer",
            "{.url {private$project$links$redcap_designer}}."
          )
        )
      }
      private$project$metadata
    },
    #' @field redcap Read-only named list with REDCap information including
    #' users and log.
    redcap = function(value) {
      if (!missing(value)) {
        cli_alert_danger(
          paste(
            "`redcap` is read only. It is generated from communication with",
            "REDCap. If you just want to work with the object more in R,",
            "reassign the object like, `redcap_log <- project$redcap$log`.",
            "Alternatively, use output from `project$generate_dataset()` See",
            "REDCap Designer {.url {private$project$links$redcap_designer}}."
          )
        )
      }
      private$project$redcap
    },
    #' @field .internal Read-only internal project object for custom workflows
    .internal  = function(value) {
      if (!missing(value)) {
        cli_alert_danger(
          "`.internal` is read only. Use public `REDCapSyncProject` methods"
        )
      }
      private$project
    }
  ),
  public = list(
    #' @description
    #' The end user will not see `project$new()`. This is handled internally.
    #' Users should construct objects using [setup_project()]. The remain
    #' methods will be accessible to any user.
    #' @param project a list object meant to be stored internally within R6
    initialize = function(project) {
      assert_setup_project(project)
      private$project <- project
      invisible(self)
    },
    #' @description Print some key project information
    print = function() {
      cli_h1("REDCapSyncProject")
      cli_text("Name: {private$project$project_name}")
      cli_text("Title: {private$project$redcap$project_info$project_title}")
      cli_text("PID: {private$project$redcap$project_id}")
      cli_text("Token Name: {private$project$token_name}")
      cli_text("Sync Frequency: {private$project$settings$sync_frequency}")
      cli_text("Last Update: {private$project$internals$last_data_update}")
      cli_text("REDCap: {.url {private$project$links$redcap_home}}")
      cli_text("Directory: {.file {private$project$dir_path}}")
      help_cli_text()
      invisible(self)
    },
    #' @description
    #' Updates the REDCap data for (`project` object) by checking REDCap log for
    #' changes. Sync is performed according to the `sync_frequency` set in
    #' [setup_project()] by default. Use `hard_check` to force a check, or `
    #' hard_reset` to force a complete refresh. As a default, this object will
    #' be saved to your directory when necessary.
    sync = function(save_datasets = TRUE,
                    save_to_dir = TRUE,
                    hard_check = FALSE,
                    hard_reset = FALSE) {
      if (private$project$internals$is_test) {
        cli_alert_info("TEST projects do not communicate with the API")
        if (save_datasets && save_to_dir) {
          private$project <- save_project_datasets(project = private$project,
                                                   hard_reset = hard_reset)
        }
        return(invisible(self))
      }
      private$project <- sync_project(project = private$project,
                                      save_datasets = save_datasets,
                                      save_to_dir = save_to_dir,
                                      hard_check = hard_check,
                                      hard_reset = hard_reset)
      invisible(self)
    },
    #' @description  Add a new dataset entry
    add_dataset = function(dataset_name,
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
                           include_records = TRUE,
                           include_users = TRUE,
                           include_log = FALSE,
                           annotate_from_log = TRUE,
                           include_comments = TRUE,
                           with_links = TRUE,
                           separate = FALSE,
                           use_csv = FALSE,
                           dir_other = NULL,
                           file_name = NULL,
                           hard_reset = FALSE) {
      private$project <- add_project_dataset(
        project = private$project,
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
        include_records = include_records,
        include_users = include_users,
        include_log = include_log,
        annotate_from_log = annotate_from_log,
        include_comments = include_comments,
        with_links = with_links,
        separate = separate,
        use_csv = use_csv,
        dir_other = dir_other,
        file_name = file_name,
        hard_reset = hard_reset
      )
      invisible(self)
    },
    #' @description  Load dataset if previously defined with `add_dataset`.
    load_dataset = function(dataset_name,
                            envir = NULL) {
      dataset_names <- names(private$project$datasets)
      assert_choice(dataset_name, dataset_names, null.ok = FALSE)
      assert_environment(envir, null.ok = TRUE)
      dataset <- REDCapSyncDataset$new(project = private$project,
                                       dataset_name = dataset_name)
      dataset$to_envir(envir = envir)
      invisible(dataset)
    },
    #' @description  Clear all or specified datasets from the `project` object.
    remove_datasets = function(dataset_names = NULL) {
      private$project <- clear_project_datasets(
        project = private$project,
        dataset_names = dataset_names
      )
      invisible(self)
    },
    #' @description  Generate [dataset] object. This is usually handled
    #' internally for some default behavior but is provided here for ad-hoc
    #' custom datasets.
    generate_dataset = function(dataset_name,
                                envir = NULL,
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
                                include_records = TRUE,
                                include_users = TRUE,
                                include_log = FALSE,
                                annotate_from_log = TRUE,
                                include_comments = FALSE) {
      assert_environment(envir, null.ok = TRUE)
      provided_dataset_name <- !missing(dataset_name)
      if (provided_dataset_name) {
        if (dataset_name %in% names(private$project$datasets)) {
          cli_alert_warning("{dataset_name} is already a defined dataset")
          cli_alert_info("It will be loaded... other paramers ignored")
        }
      } else {
        dataset_name <- "custom"
        i <- 0L
        while (dataset_name %in% names(private$project$datasets)) {
          i <- i + 1L
          dataset_name <- paste0("custom", i)
        }
        cli_alert_warning("`dataset_name` not provided: Using '{dataset_name}'")
      }
      dataset <- REDCapSyncDataset$new(
        project = private$project,
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
      dataset$to_envir(envir = envir)
      invisible(dataset)
    },
    #' @description saves datasets to Excel via setting provided to
    #' `add_dataset`.
    save_datasets = function(hard_reset = FALSE) {
      first_stamp <- private$project$internals$last_dataset_save
      private$project <- save_project_datasets(project = private$project,
                                               hard_reset = hard_reset)
      second_stamp <- private$project$internals$last_dataset_save
      was_updated <- !identical(first_stamp, second_stamp)
      if (was_updated) {
        #consider separating just saving last in details
        private$project <- save_project(private$project)
      }
      invisible(self)
    },
    #' @description saves dataset to Excel via setting provided to
    #' `add_dataset`.
    save_dataset = function(dataset_name) {
      dataset_names <- names(private$project$datasets)
      assert_choice(dataset_name, dataset_names, null.ok = FALSE)
      private$project <- save_project_dataset(project = private$project,
                                              dataset_name = dataset_name)
      invisible(self)
    },
    #' @description  Save project object to directory chosen with
    #' [setup_project].
    save = function() {
      if (private$project$internals$is_test) {
        cli_alert_info("TEST projects do not save to directories!")
        return(invisible(self))
      }
      private$project <- save_project(private$project)
      invisible(self)
    },
    #' @description  Set keyring token. See vignette and config for detail.
    set_keyring_token = function() {
      if (private$project$internals$is_test) {
        cli_alert_info("TEST projects do not save to directories!")
        return(invisible(self))
      }
      set_project_keyring_token(private$project)
      invisible(self)
    },
    #' @description test connection via communication with API.
    test_token = function() {
      if (private$project$internals$is_test) {
        cli_alert_info("TEST projects do not communicate with the API")
        return(invisible(self))
      }
      get_project_token(project = private$project, silent = FALSE)
      private$project <- test_project_token(project = private$project)
      invisible(self)
    },
    #' @description opens links in browser.
    url_launch = function(link_type = "home",
                          open_browser = TRUE) {
      if (private$project$internals$is_test && open_browser) {
        cli_alert_info("TEST projects do not link to the web!")
        return(invisible(self))
      }
      the_link <- get_project_url(private$project,
                                  link_type = link_type,
                                  open_browser = open_browser)
      if (!open_browser) {
        return(the_link)
      }
      invisible(self)
    },
    #' @description opens record links in browser.
    url_record_launch = function(record = NULL,
                                 page = NULL,
                                 instance = NULL,
                                 open_browser = TRUE) {
      if (private$project$internals$is_test && open_browser) {
        cli_alert_info("TEST projects do not link to the web!")
        return(invisible(self))
      }
      the_link <- get_record_url(private$project,
                                 record = record,
                                 page = page,
                                 instance = instance,
                                 open_browser = open_browser)
      if (!open_browser) {
        return(the_link)
      }
      invisible(self)
    },
    #' @description
    #' This will only overwrite and new data. It will not directly delete any
    #' data. Because this is a function that can mess up your data, use it
    #' very carefully. Remember all changes are saved in the REDCap log if
    #' there's an issue. Missing rows and columns are allowed!
    upload = function(to_be_uploaded) {
      if (private$project$internals$is_test) {
        cli_alert_info("TEST projects do not communicate with the API")
        return(invisible(self))
      }
      # add detect labelled vs raw?
      if (!is_named_df_list(to_be_uploaded, strict = TRUE)) {
        if (!is.data.frame(to_be_uploaded)) {
          cli_abort(
            paste0(
              "`to_be_uploaded` must be a date.frame or named list of ",
              "data.frames!"
            )
          )
        }
        if (!is_something(to_be_uploaded)) {
          cli_alert_warning("Nothing to upload...")
          return(invisible(self))
        }
        to_be_uploaded <- list(upload = to_be_uploaded)
      }
      id_col <- private$project$metadata$id_col
      refresh_records <- extract_values_from_form_list(to_be_uploaded, id_col)
      is_labelled <- private$project$settings$labelled
      for (upload_name in names(to_be_uploaded)) {
        upload_this <- to_be_uploaded[[upload_name]]
        # add comparison check but need to test...
        if (is_labelled) {
          upload_this <- labelled_to_raw_form(form = upload_this,
                                              project = private$project)
        }
        upload_form_to_redcap(
          to_be_uploaded = upload_this,
          project = private$project,
          batch_size = private$project$settings$batch_size_upload
        )
      }
      Sys.sleep(3L)
      private$project <- sync_project_check(
        project = private$project,
        hard_reset = FALSE,
        records = refresh_records
      )
      invisible(invisible(self))
    }
  ),
  private = list(
    project = NULL
  ),
  cloneable = FALSE
)
