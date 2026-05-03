#' @title Synchronizing REDCap Project
#' @description
#' [R6][R6::R6Class] project object for [REDCapSync] This is the main class for
#' managing REDCap data, metadata, and sync operations. Users should construct
#' objects using [setup_project()]. To reopen an existing project,
#' use [load_project()].
#' @details
#' The methods documented below are functions that work without having to use
#' "<-". For example, if you load a project with
#' \code{project <- load_project("TEST_CLASSIC")}, and then then run
#' \code{project$sync()}, then the project object will contain the updated data.
#' That function actually invisibly returns itself which allows for "chaining",
#' such as \code{load_project("TEST_CLASSIC")$sync()}. More features are being
#' developed that will allow for the addition of fields (outside of REDCap).
#'
#' @param dataset_name Character. The name of the configured dataset from which
#' to generate the dataset. *If you provide `dataset_name` all other parameters
#' are inherited according to what was set with `add_dataset`.
#' @param transformation_type Character scalar. How to transform data for the
#' dataset. Default is "default". Other options are "none" and
#' "merge_non_repeating". "default" first merges non-repeating and if there are
#' repeating forms, it also merges non-repeating variables to the right.
#' "none" does not transform anything. "merge_non_repeating" still merges
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
#' @param hard_reset Logical. If `TRUE`, overwrite existing dataset files
#' with the same name. Default is `FALSE`.
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
#' @param form string of raw REDCap form name, such as "survey_one".
#' @param link_type Character. Type of REDCap URL to retrieve. Choose one of
#' "base", "home", "record_home", "records_dashboard", "api",
#' "api_playground", "codebook", "user_rights", "setup", "logging",
#' "designer", "dictionary", "data_quality", or "identifiers".
#' @param open_browser Logical. If TRUE, launches the link in the default
#'   browser.
#' @param record character of record
#' @param page character of page (instrument/form)
#' @param instance character of instance
#' @param save_to_dir Logical (TRUE/FALSE). If TRUE, saves the updated data in
#' the project object to the directory at `dir_path`. Ignored when `dir_path` is
#'  `NULL`. Default is `TRUE`.
#' @param save_datasets Logical (TRUE/FALSE). If TRUE, saves the datasets
#' that were previously added during a sync. Default is `TRUE`.
#' @param hard_check Will check REDCap even if not due (see `sync_frequency`
#' parameter from `setup_project()`)
#' @param type Character. The metadata component to request. One of "fields",
#' "forms", or "choices".
#' @param dataset_names One or more dataset names. Default is `NULL`.
#' @param to_be_uploaded data.frame in raw coded form to upload.
#' uploading to REDCap. Default is 500L.
#' @examples
#' project <- load_project("TEST_CLASSIC")
#' project$sync()
#' project$save()
#'
#' @seealso
#' \link{setup_project} for initializing the `project` object.'
#' @returns
#' An R6ClassGenerator which is used internally to create or load a
#' project object for the user
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
        cli_alert_wrap(
          paste(
            "`metadata` is read only. To change, do so on the ",
            "REDCap website, or use the REDCap API. If you just want to work",
            "with the object more in R, reassign the object like,",
            "`fields <- project$metadata$fields`. Alternatively use the output",
            "from `project$generate_dataset()`"
          ),
          bullet_type = "x",
          url = private$project$links$redcap_designer
        )
      }
      private$project$metadata
    },
    #' @field redcap Read-only named list with REDCap information including
    #' users and log.
    redcap = function(value) {
      if (!missing(value)) {
        cli_alert_wrap(
          paste(
            "`redcap` is read only. It is generated from communication with",
            "REDCap. If you just want to work with the object more in R,",
            "reassign the object like, `redcap_log <- project$redcap$log`.",
            "Alternatively use the output from `project$generate_dataset()`"
          ),
          bullet_type = "x",
          url = private$project$links$redcap_designer
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
      if (private$project$internals$is_test) {
        cli_alert_info("TEST projects do not save to directories!")
        return(invisible(self))
      }
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
          stop("`to_be_uploaded` must be a date.frame or named list of ",
               "data.frames!")
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
      private$project <- sync_project_refresh(
        project = private$project,
        refresh_records = refresh_records
      )
      invisible(invisible(self))
    }
  ),
  private = list(
    project = NULL
  ),
  cloneable = FALSE
)
