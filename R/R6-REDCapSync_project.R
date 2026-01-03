#' @title REDCapSync Encapsulated Project Object
#' @description
#' [R6][R6::R6Class] project object for [REDCapSync]
#' Main class for managing REDCap data, metadata, and sync operations.
#' Users should construct objects using [setup_project()]. For exisiting
#' projects, use [load_project()]. For offline examples use
#' [load_test_project()].
#' @param summary_name Character. The name of the summary from which to generate
#' the summary. *If you provide `summary_name` all other parameters are
#' inherited according to what was set with `add_project_summary`.
#' @param transformation_type Character vector.
#' Default is "default".
#' Also have "none", "flat", "merge_non_repeating"
#' "default" first merges non-repeating and if there are repeating forms it
#' merges non-repeating variables to the right of repeating instruments
#' "flat" is one-record, one-row, even if there are repeating forms
#' "none" does not transform anything
#' "merge_non_repeating" still merges all non-repeating instruments but
#' does not merge them to repeating instruments
#' @param merge_form_name A character string representing the name of the merged
#' form. Default is "merged".
#' @param filter_field Character. The name of the field in the database to
#' filter on.
#' @param filter_choices Vector. The values of `filter_field` used to define the
#' summary.
#' @param filter_list Vector. The values of `filter_field` used to define the
#' summary.
#' @param filter_strict Logical. If `TRUE`, all forms will be filtered by
#' criteria. If `FALSE`, will convert original filter to id column and filter
#' all other forms by that record. Default is `TRUE`.
#' @param form_names Character vector. Names of forms to include in the summary.
#' Default is `NULL`, which includes all forms.
#' @param field_names Character vector. Names of specific fields to include in
#' the summary. Default is `NULL`, which includes all fields.
#' @param exclude_identifiers Logical. Whether to exlude identifiers in the data
#' in the summary. Default is `TRUE`.
#' @param exclude_free_text Logical for excluding free text. Default is `FALSE`.
#' @param date_handling character string. One of `none`,`exclude_dates`,
#' `random_shift_by_record`, `random_shift_by_project`, `zero_by_record`, or
#' `zero_by_project` random shift is +/- 90 unless changed with options
#' @param labelled Logical. If `TRUE`, the data will be converted to labelled.
#' Default is `TRUE`.
#' @param clean Logical. If `TRUE`, the data will be cleaned before summarizing.
#' Default is `TRUE`. If missing codes present AND number or date type, R will
#' convert to those to NA and would make that variable not upload compatible
#' @param drop_blanks Logical. If `TRUE`, records with blank fields will be
#' dropped. Default is `TRUE`.
#' @param drop_missing_codes Logical. If `TRUE`, will convert missing codes
#' to NA. Default is `FALSE`.
#' @param drop_others Character vector of other values that should be dropped.
#' @param include_metadata Logical. If `TRUE`, metadata will be included in the
#' summary. Default is `TRUE`.
#' @param include_users Logical. If `TRUE`, user-related information will be
#' included in the summary. Default is `TRUE`.
#' @param include_records Logical. If `TRUE`, a record summary will be
#' included in the generated summary. Default is `TRUE`.
#' @param include_log Logical. If `TRUE`, the log of changes will be included in
#' the summary. Default is `TRUE`.
#' @param annotate_from_log Logical. If `TRUE`, the metadata, users, and records
#' will be annotated using the log. Default is `TRUE`.
#' @param internal_use A logical flag (`TRUE` or `FALSE`). If `TRUE`, then will
#' return data_list meant for internal use. Defaults to `FALSE`.
#' @param hard_reset Logical. If `TRUE`, overwrite existing summary files
#' with the same name. Default is `FALSE`.
#' @param with_links Optional logical (TRUE/FALSE) for including links in Excel
#' sheets. Default is `FALSE`.
#' @param separate Optional logical (TRUE/FALSE) separating each form into
#' separate files as opposed to multi-tab Excel. Default is `FALSE`.
#' @param use_csv Logical (TRUE/FALSE). If TRUE, uses CSV files for data
#' storage. Default is `FALSE`
#' @param dir_other Character. The directory where the summary file will be
#' saved. Default is the `output` folder within the database directory.
#' @param file_name Character. The base name of the file where the summary will
#' be saved. Default is `<project_name>_<summary_name>`.
#' @param envir environment variable
#' @param form string of raw form name such as "survey_one"
#' @param link_type choose one of "base", "home", "record_home",
#' "records_dashboard", "api", "api_playground", "codebook", "user_rights",
#' "setup", "logging", "designer", "dictionary", "data_quality", "identifiers"
#' @param open_browser logical for launching the link in internet browser
#' @param summarize Logical (TRUE/FALSE). If TRUE, summarizes data to directory.
#' @param save_to_dir Logical (TRUE/FALSE). If TRUE, saves the updated data to
#' the directory. Default is `TRUE`.
#' @param hard_check Will check REDCap even if not due (see `sync_frequency`
#' parameter from `setup_project()`)
#' @param type string of either "fields","forms", or "choices"
#' @param field_name Character. The name of the field to which the
#' transformation
#' will be applied.
#' @param form_name Character. The name of the form containing the field.
#' @param field_type Character. The type of the field in REDCap (e.g., "text",
#' "checkbox", "dropdown").
#' @param field_type_R Character. The corresponding R data type for the field.
#' Default is `NA`.
#' @param field_label Character. The label for the field. Default is `NA`.
#' @param select_choices_or_calculations Character. A string specifying the
#' choices (for dropdown, radio, or checkbox fields) or calculations (for
#' calculated fields). Default is `NA`.
#' @param field_note Character. An optional note or comment for the field.
#' Default is `NA`.
#' @param identifier Character. A string indicating whether the field is an
#' identifier (e.g., "Y" for yes). Default is an empty string (`""`).
#' @param units Character. The units of measurement for the field, if
#' applicable. Default is `NA`.
#' @param data_func Function or NA. An optional function to transform or
#' validate the data in the field. Default is `NA`.
#' @param summary_names One or more summary names. Default is `NULL`.
#' @param to_be_uploaded data.frame in raw coded form.
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return An R6ClassGenerator
#' @keywords internal
REDCapSync_project <- R6Class(
  "REDCapSync_project",
  #' @description
  #' Active binding are read-only
  active = list(
    #' @field project_name Read-only character string of project_name as
    #' assigned from [setup_project].
    project_name = function(value) {
      if (!missing(value)) {
        message(
          "`project_name` is read-only. To change `setup_project()`"
        )
      }
      private$project$project_name
    },
    #' @field data Read-only named list where each name is an instrument name.
    #' See [REDCapSync::REDCapSync_project] or [REDCapSync::REDCapSync_project$sync()]
    #' or see \href{../../REDCapSync/html/REDCapSync_project.html#method-sync}{\code{REDCapSync_project$sync()}}
    #' or see \href{../../REDCapSync/reference/REDCapSync_project.html#method-REDCapSync_project-sync}{\code{REDCapSync_project$sync()}}
    data = function(value) {
      if (!missing(value)) {
        print(value)
        message(
          "`data` is read-only. To change REDCap data either use project$upload() or work with an output by assigning the data with `form_to_edit <-project$data$<form_name>`"
        )
      }
      private$project$data
    },
    #' @field .internal_project (`character(1)`)\cr
    #' Entire internal project object for more advanced/custom workflows
    .internal_project  = function(value) {
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
      invisible()
    },
    #' @description Print project metadata
    info = function() {
      message("Project Name: ", private$project$project_name)
      message("Directory: ", private$project$dir_path)
      message("Last Data Update: ", private$project$internals$last_data_update)
    },
    #' @description
    #' Updates the REDCap data for (`project` object) by checking REDCap log.
    #' @return Messages for confirmation.
    #' @seealso
    #' \link{setup_project} for initializing the `project` object.'
    sync = function(summarize = TRUE,
                    save_to_dir = TRUE,
                    hard_check = FALSE,
                    hard_reset = FALSE) {
      private$project <- sync_project(
        project = private$project,
        summarize = summarize,
        save_to_dir = save_to_dir,
        hard_check = hard_check,
        hard_reset = hard_reset)
      invisible(self)
    },
    #' @description  Add a new summary entry
    add_summary = function(summary_name,
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
                           with_links = TRUE,
                           separate = FALSE,
                           use_csv = FALSE,
                           dir_other = NULL,
                           file_name = NULL,
                           hard_reset = FALSE) {
      private$project <- add_project_summary(
        project = private$project,
        summary_name = summary_name,
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
        with_links = with_links,
        separate = separate,
        use_csv = use_csv,
        dir_other = dir_other,
        file_name = file_name,
        hard_reset = hard_reset
      )
      invisible(self)
    },
    #' @description  Clear all project summaries
    remove_summaries = function(summary_names = NULL) {
      private$project <- clear_project_summaries(
        project = private$project,
        summary_names = summary_names
      )
      invisible(self)
    },
    #' @description  Add a new summary entry
    generate_summary = function(summary_name = "REDCapSync", envir = NULL) {
      assert_environment(envir, null.ok = TRUE)
      summary_names <- private$project$summary |>
        names() |>
        setdiff("all_records")
      assert_choice(summary_name, summary_names, null.ok = FALSE)
      project_summary <-
        generate_project_summary(
          project = private$project,
          summary_name = summary_name
        )
      if (!is.null(envir)) {
        list2env(project_summary, envir = envir)
      }
      invisible(project_summary)
    },
    #' @description  Add a new summary entry
    add_field = function(field_name,
                         form_name,
                         field_type,
                         field_type_R = NA,
                         field_label = NA,
                         select_choices_or_calculations = NA,
                         field_note = NA,
                         identifier = "",
                         units = NA,
                         data_func = NA) {
      message("Added field! (placeholder)")
      invisible(self)
    },
    #' @description  Removes summary entry
    remove_fields = function(field_names = NULL) {
      message("Removed field! (placeholder)")
      invisible(self)
    },
    #' @description summarize project and save to Excel
    summarize = function(hard_reset = FALSE) {
      first_stamp <- private$project$internals$last_summary
      private$project <-
        summarize_project(project = private$project, hard_reset = hard_reset)
      second_stamp <- private$project$internals$last_summary
      was_updated <- !identical(first_stamp, second_stamp)
      if (was_updated) {
        #consider separating just saving last in details
        private$project <- save_project(private$project)
      }
      invisible(self)
    },
    #' @description save summary to Excel
    save_summary = function(summary_name) {
      summary_names <- private$project$summary |>
        names() |>
        setdiff("all_records")
      assert_choice(summary_name, summary_names, null.ok = FALSE)
      private$project <- save_summary(project = private$project, summary_name)
      invisible(self)
    },
    #' @description  Add a new summary entry
    save = function() {
      private$project <- save_project(private$project)
      invisible(self)
    },
    #' @description  Returns list of data or the specified form.
    show_metadata = function(type = NULL, envir = NULL) {
      assert_environment(envir, null.ok = TRUE)
      assert_choice(type, c("fields", "forms", "choices"), null.ok = TRUE)
      return_this <- private$project$metadata
      if (!is.null(type)) {
        #add warning or message?
        return_this <- private$project$metadata[type]
      }
      if (!is.null(envir)) {
        # add check for conflicts?
        .GlobalEnv |> ls()
        return_this |>
          process_df_list(silent = TRUE) |>
          list2env(envir = envir)
      }
      invisible(return_this)
    },
    #' @description  Returns list of data or the specified form.
    show_data = function(form = NULL, envir = NULL) {
      assert_environment(envir, null.ok = TRUE)
      return_this <- private$project$data
      if (!is.null(form)) {
        #add warning or message?
        return_this <- private$project$data[[form]]
      }
      if (!is.null(envir)) {
        list2env(return_this, envir = envir)
      }
      invisible(return_this)
    },
    #' @description
    #' Displays the REDCap API token currently stored in the session as an
    #' environment variable. It's essentially a wrapper for
    #' Sys.getenv("YOUR_TOKEN_NAME"), but it also validates that the token is
    #' formatted like a REDCap token and provides messgaes if not valid.
    #' The token is not returned as an R object to maintain security.
    show_token = function() {
      view_project_token(private$project)
    },
    #' @description test connection via communication with API
    test_token = function() {
      private$project <- test_project_token(private$project)
      invisible(self)
    },
    #' @description opens links in browser
    url_launch = function(link_type = "home",
                          open_browser = TRUE) {
      get_project_url(private$project,
                      link_type = link_type,
                      open_browser = open_browser)
    },
    #' @description
    #' This will only overwrite and new data. It will not directly delete any
    #' data. Because this is a function that can mess up your data, use it
    #' very carefilly. Remember all changes are saved in the REDCap log if
    #' there's an issue. Missing rows and columns are allowed!
    upload = function(to_be_uploaded, batch_size = 500L) {
      # add detect labelled vs raw?
      if (!is_named_df_list(to_be_uploaded, strict = TRUE)) {
        if (!is.data.frame(to_be_uploaded)) {
          stop("`to_be_uploaded` must be a date.frame or named list of ",
               "data.frames!")
        }
        if (!is_something(to_be_uploaded)) {
          cli_alert_warning("Nothing to upload...")
          return(invisible(FALSE))
        }
        to_be_uploaded <- list(upload = to_be_uploaded)
      }
      is_labelled <- private$project$internals$labelled
      for (upload_name in names(to_be_uploaded)) {
        upload_this <- to_be_uploaded[[upload_name]]
        # add comparison check but need to test...
        if (is_labelled) {
          upload_this <- labelled_to_raw_form(
            form = upload_this,
            project = private$project)
        }
        upload_form_to_redcap(
          to_be_uploaded = upload_this,
          project = private$project,
          batch_size = batch_size
        )
      }
      # add check for uploaded something
      private$project <- sync_project(
        project = private$project,
        # summarize = summarize,
        # save_to_dir = save_to_dir,
        hard_check = TRUE
        # hard_reset = hard_reset
      )
      invisible(TRUE) #maybe give TRUE FALSE here instead of self
    },
    #' @description  returns internal list
    .internal = function() {
      invisible(private$project)
    }
  ),
  private = list(
    project = NULL
  ),
  cloneable = FALSE
)
