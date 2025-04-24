#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your
#' own risk.
#' Remember all changes are saved in the redcap log if there's an issue. Missing
#' rows and columns are fine!
#' @inheritParams save_project
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean
#' data pass your data to `labelled_to_raw_form(form,project)` first.
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
#' @export
upload_form_to_REDCap <- function(to_be_uploaded, project, batch_size = 500) {
  REDCapR::redcap_write(
    ds_to_write = to_be_uploaded %>% all_character_cols(),
    batch_size = batch_size,
    interbatch_delay = 0.2,
    continue_on_error = FALSE,
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    overwrite_with_blanks = TRUE
  )
}
#' @title Upload from your directory to REDCap
#' @description
#' This function is designed to upload changes from a locally modified `project`
#' object to REDCap. It should be run after using
#' `project_import <- read_from_REDCap_upload(project)`.
#' The function compares the imported data (`project_import`) to the existing
#' data in `project` and only uploads new or changed data. It will not directly
#' delete any data.
#'
#' This function has the potential to modify your data, so it should be used
#' cautiously. Any changes made through this function will be logged in the
#' REDCap log.
#'
#' @inheritParams save_project
#' @param batch_size Numeric. The number of records to upload in each batch.
#' Default is 500.
#' @param ask Logical. If TRUE, the function will prompt you to preview the data
#' that will be uploaded before proceeding. Defaults to TRUE.
#' @param view_old Logical. If TRUE, the function will show a preview of the old
#' records before upload. Defaults to TRUE.
#' @param n_row_view Numeric. The number of rows to display when previewing old
#' data. Default is 20.
#'
#' @return A series of messages indicating the progress and status of the
#' upload.
#'
#' @details
#' This function uploads changes to a REDCap project, based on the differences
#' between the locally imported data (`project_import`) and the existing data
#' (`project`).
#' It uploads changes in batches as specified by `batch_size` and allows you to
#' preview the changes before the upload if `ask` is set to TRUE.
#' It will not delete any data from REDCap, and it is intended to only upload
#' new or modified records. This function should be used with caution. Any
#' changes during the upload will be logged in the REDCap system log.
#' @export
upload_project_to_REDCap <- function(project, batch_size = 500, ask = TRUE, view_old = TRUE, n_row_view = 20) {
  warning("This function is not ready for primetime yet! Use at your own risk!", immediate. = TRUE)
  project <- assert_blank_project(project)
  # if(ask){
  #   if(count_project_upload_cells(project)>5000){
  #     choice  <- utils::menu(choices = c("YES - Move forward with larger upload","NO - I want to stop and double check what I'm about to upload"),title = "This is a large upload. Do you want to proceed?")
  #     if(choice!=1)stop("Double check project object prior to upload")
  #   }
  # }
  warning("Right now this function only updates repeating forms. It WILL NOT clear repeating form instances past number 1. SO, you will have to delete manually on REDCap.", immediate. = TRUE)
  any_updates <- FALSE
  data_updates <- project$data_updates
  data_updates_transformation <- project$transformation$data_updates
  for (form_name in names(data_updates)) {
    to_be_uploaded <- find_upload_diff(
      to_be_uploaded = data_updates[[form_name]],
      project,
      view_old = view_old,
      n_row_view = n_row_view
    )
    data_updates[[form_name]] <- to_be_uploaded
    if (is_something(to_be_uploaded)) {
      if (project$internals$labelled) {
        to_be_uploaded <- to_be_uploaded %>% labelled_to_raw_form(project)
      }
      do_it <- 1
      if (ask) {
        do_it <- utils::menu(choices = c("Yes upload", "No and go to next"), title = "Do you want to upload this?")
      }
      if (do_it == 1) {
        upload_form_to_REDCap(to_be_uploaded = to_be_uploaded, project = project, batch_size = batch_size)
        data_updates[[form_name]] <- NULL
        any_updates <- TRUE
        project$internals$last_data_update <- now_time()
      }
    }
  }
  if(!is_something(data_updates)){
    data_updates <- list()
  }
  for (form_name in names(data_updates_transformation)) {
    to_be_uploaded <- find_upload_diff(
      to_be_uploaded = data_updates_transformation[[form_name]],
      project,
      view_old = view_old,
      n_row_view = n_row_view
    )
    data_updates_transformation[[form_name]] <- to_be_uploaded
    if (is_something(to_be_uploaded)) {
      if (project$internals$labelled) {
        to_be_uploaded <- to_be_uploaded %>% labelled_to_raw_form(project)
      }
      do_it <- 1
      if (ask) {
        do_it <- utils::menu(choices = c("Yes upload", "No and go to next"), title = "Do you want to upload this?")
      }
      if (do_it == 1) {
        upload_form_to_REDCap(to_be_uploaded = to_be_uploaded, project = project, batch_size = batch_size)
        data_updates_transformation[[form_name]] <- NULL
        any_updates <- TRUE
        project$internals$last_data_update <- now_time()
      }
    }
  }
  if(!is_something(data_updates_transformation)){
    data_updates_transformation <- list()
  }
  project$data_updates <- data_updates
  project$transformation$data_updates <-  data_updates_transformation
  if (any_updates) {
    project <- sync_project(project)
  }
  invisible(project)
}
#' @title Find the project_import and project differences
#' @description
#' This function compares the data in the `project` object (new data) with the
#' previous or reference data to identify differences. It returns a list of
#' differences for upload. The function ensures that the new data matches the
#' structure defined by the metadata and provides warnings when discrepancies
#' are found.
#' @param to_be_uploaded a data.frame or list of data.frames to be uploaded
#' @inheritParams save_project
#' @param view_old Logical. If TRUE, it will display a preview of the old data
#' (default is FALSE).
#' @param n_row_view Numeric. Defines how many rows of the old data to view
#' (default is 20).
#' @return A list of differences between the new and old data (`upload_list`).
#' @details
#' The function compares the data in `project$data_updates` (new data) with the
#' current data in the database (`project$data`). If the form names in the new
#' data do not match the `project$metadata$forms$form_name`, a warning is
#' issued. The function goes through each table in the new data and compares it with the
#' old data, recording the differences.
#'
#' The `compare` and `to` parameters allow users to specify specific data
#' choices to compare, though their exact usage will depend on how the function
#' is fully implemented.
#' @export
find_upload_diff <- function(to_be_uploaded,project, view_old = FALSE, n_row_view = 20) {
  project <- assert_blank_project(project)
  old_list <- list()
  # if (any(!names(new_list) %in% project$metadata$forms$form_name)) warning("All upload names should ideally match the project form names, `project$metadata$forms$form_name`", immediate. = TRUE)
  already_used <- NULL
  if(is.data.frame(to_be_uploaded)){
    to_be_uploaded <- list(upload_me = to_be_uploaded)
  }
  for (user_name in names(to_be_uploaded)) { # form_name <- names(new_list) %>% sample(1)
    new <- to_be_uploaded[[user_name]]
    ref_cols <- project$redcap$raw_structure_cols
    ref_cols <- ref_cols[which(ref_cols %in% colnames(new))]
    data_cols <- colnames(new)[which(!colnames(new) %in% ref_cols)]
    form_names <- field_names_to_form_names(project, data_cols)
    form_name_old <- form_names
    # if (any(form_names %in% already_used)) {
    #   stop("REDCapSync will not allow you to upload items from same form multiple times in one loop without refreshing.")
    # }
    if(length(form_names)>1){
      if(any(!form_names %in% project$metadata$forms$form_name[which(project$metadata$forms$repeating)])){
        stop("Can't have variables in multiple forms in an upload data.frame unless it is a non-repeating form")
      }
      stop("Can only upload data from one form at a time.")
    }
    keep <- c(ref_cols,data_cols %>% vec1_in_vec2(form_names_to_field_names(form_names = form_names, project = project)))
    drop <- data_cols %>% vec1_not_in_vec2(form_names_to_field_names(form_names = form_names, project = project))
    if (length(drop) > 0) {
      message("Dropping field_names that aren't part of REDCap metadata: ", paste0(drop, collapse = ", "))
    }
    to_be_uploaded[[user_name]] <- find_form_diff2(
      new = new[,keep],
      old = project$data[[form_names]][,keep],
      ref_cols = ref_cols,
      message_pass = paste0(user_name, ": "),
      view_old = view_old,
      n_row_view = n_row_view
    )
  }
  if (is_something(to_be_uploaded)) {
    return(invisible(to_be_uploaded))
  }
  message("No upload updates!")
  invisible(NULL)
}
#' @noRd
check_field <- function(project, form, field_name, autofill_new = TRUE) {
  form <- field_names_to_form_names(project, field_name)
  records <- form[[project$redcap$id_col]] %>% unique()
  bad_records <- records[which(!records %in% project$summary$all_records[[project$redcap$id_col]])]
  if (length(bad_records) > 0) stop("Records not included in project: ", records %>% paste0(collapse = ", "))
  cols_mandatory_structure <- project$metadata$form_key_cols[[form]]
  cols_mandatory <- c(cols_mandatory_structure, field_name)
  old <- project$data[[form]][, cols_mandatory]
  old <- old[which(old[[project$redcap$id_col]] %in% records), ]
  new <- form
  missing_structure_cols <- cols_mandatory[which(!cols_mandatory %in% colnames(new))]
  cols <- cols_mandatory[which(cols_mandatory %in% colnames(new))]
  new <- new[, cols]
  included_records <- records[which(records %in% old[[project$redcap$id_col]])]
  if (length(missing_structure_cols) > 0) {
    included_records_many_rows <- included_records[which(included_records %>% lapply(function(record) {
      length(which(old[[project$redcap$id_col]] == record)) > 1
    }) %>% unlist())]
    if (length(included_records_many_rows) > 0) stop("form is missing structural columns (", missing_structure_cols %>% paste0(collapse = ", "), ") and has ", form, " rows with multiple entries... remove them or add the intended columns: ", included_records_many_rows %>% paste0(collapse = ", "))
    if ("redcap_repeat_instrument" %in% missing_structure_cols) new$redcap_repeat_instrument <- form
    if ("redcap_repeat_instance" %in% missing_structure_cols) {
      new$redcap_repeat_instance <- new[[project$redcap$id_col]] %>%
        lapply(function(record) {
          if (record %in% included_records) {
            return(old$redcap_repeat_instance[which(old[[project$redcap$id_col]] == record)])
          }
          "1"
        }) %>%
        unlist()
    }
    # add event?
  }
  z <- new %>% find_form_diff2(old, ref_cols = cols_mandatory_structure)
  if (!is.null(z)) {
    i_of_old_name_change <- which(!colnames(old) %in% cols_mandatory_structure)
    colnames(old)[i_of_old_name_change] <- paste0(colnames(old)[i_of_old_name_change], "_old")
    z_old <- z %>% merge(old, by = cols_mandatory_structure)
    # add autoallow NA
    if (nrow(z) > 0) {
      # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
      choices <- c("upload new", "keep old", "manual entry", "launch redcap link only")
      for (i in seq_len(nrow(z))) {
        form <- z[i, ]
        x <- z_old[i, ]
        new_answer <- x[[field_name]]
        old_answer <- x[[paste0(field_name, "_old")]]
        ask <- TRUE
        if (autofill_new) {
          if (is.na(old_answer) && !is.na(new_answer)) {
            ask <- FALSE
          }
        }
        if (ask) {
          print.data.frame(z_old[i, ])
          choice <- utils::menu(choices, title = paste0("What would you like to do?"))
        } else {
          choice <- 1
        }
        if (choice == 1) {
          form %>%
            labelled_to_raw_form(project) %>%
            upload_form_to_REDCap(project)
          message("Uploaded: ", form %>% paste0(collapse = " | "))
        }
        if (choice == 2) {
          message("Did not change anything")
        }
        if (choice == 3) {
          project %>% link_REDCap_record(form[[project$redcap$id_col]])
          form[[field_name]] <- readline("What would you like it to be? ")
          print.data.frame(form)
          form %>%
            labelled_to_raw_form(project) %>%
            upload_form_to_REDCap(project)
        }
        if (choice == 4) { # account for repeat? instance
          project %>% link_REDCap_record(form[[project$redcap$id_col]], form, instance = form[["redcap_repeat_instance"]])
        }
      }
    }
  }
}
#' @title Edit REDCap Data While Viewing
#' @description
#' Allows for editing a specific field in a REDCap project while simultaneously
#' viewing the corresponding records and fields from other forms. Supports
#' viewing and updating records individually, with flexible field selection.
#'
#' @inheritParams save_project
#' @param optional_form Optional data frame. A data frame containing the data to
#' be edited. If not provided, the function will pull the data from the REDCap
#' database using the specified `field_name_to_change`.
#' @param records Character or numeric vector. The records to be edited. If not
#' provided, the function will use the unique values from the specified forms.
#' @param field_name_to_change Character. The field name to be changed in the
#' REDCap database.
#' @param field_names_to_view Optional character vector. A list of field names
#' to view alongside the field being edited. Defaults to `NULL`, in which case only
#' the field being changed will be viewed.
#' @param upload_individually Logical. If `TRUE`, each change is uploaded
#' individually. Default is `TRUE`.
#'
#' @return
#' A modified `project` object with changes to the specified field(s) in the
#' REDCap project.
#'
#' @details
#' This function is useful when you want to edit specific fields in a REDCap
#' project while also reviewing related data from other forms in the project.
#' The `field_name_to_change` must be provided, and you can also specify
#' additional fields to view while editing. The data is either passed through
#' `optional_form` or pulled from the project based on the provided field names.
#'
#' @seealso
#' \code{\link{save_project}} for saving the modified database.
#'
#' @export
edit_REDCap_while_viewing <- function(project,
                                      optional_form,
                                      records,
                                      field_name_to_change,
                                      field_names_to_view = NULL,
                                      upload_individually = TRUE) {
  change_form <- field_names_to_form_names(project, field_name_to_change)
  view_forms <- field_names_to_form_names(project, field_names_to_view)
  field_names_to_view <- c(field_name_to_change, field_names_to_view) %>% unique()
  # if(length(view_forms)>1)stop("only one form combinations are allowed.")
  if (missing(records)) records <- project$data[[view_forms]][[project$redcap$id_col]] %>% unique()
  # all_forms <- c(change_form, view_forms) %>% unique()
  ref_cols_change <- project$metadata$form_key_cols[[change_form]]
  # ref_cols_view <- project$metadata$form_key_cols[[view_forms]]
  if (missing(optional_form)) {
    optional_form <- project[["data"]][[change_form]][, unique(c(ref_cols_change, field_names_to_view))]
  }
  if (is.null(field_names_to_view)) field_names_to_view <- colnames(optional_form)
  # if(any(!ref_cols%in%colnames(form)))stop("form must contain all ref_cols")
  if (length(records) > 0) {
    # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
    rows_of_choices <- which(project$metadata$choices$field_name == field_name_to_change)
    has_choices <- length(rows_of_choices) > 0
    choices1 <- c("Do Nothing", "Edit", "Launch Redcap Link Only")
    if (has_choices) {
      choices2 <- c("Do Nothing", project$metadata$choices$name[rows_of_choices], "Launch Redcap Link Only")
    } else {
      choices2 <- c("Do Nothing", "Manual Entry", "Launch Redcap Link Only")
    }
    is_repeating_form <- change_form %in% project$metadata$forms$form_name[which(project$metadata$forms$repeating)]
    form <- NULL
    form_change <- project$data[[change_form]]
    row.names(form_change) <- NULL
    form_change <- form_change[, unique(c(ref_cols_change, field_name_to_change))]
    for (record in records) { # record <- records%>% sample(1)
      record_was_updated <- FALSE
      form_view <- optional_form[which(optional_form[[project$redcap$id_col]] == record), ]
      form_view_simp <- form_view[, unique(c(project$redcap$id_col, field_names_to_view))] %>% unique()
      row.names(form_view_simp) <- NULL
      form_view_simp %>%
        t() %>%
        print()
      if (nrow(form_change) == 0) {
        print("Nothing in form_change. If you choose edit it will add an instance...")
        blank_row <- data.frame(
          record
        )
        colnames(blank_row)[[1]] <- project$redcap$id_col
        if ("redcap_repeat_instance" %in% ref_cols_change) {
          blank_row$redcap_repeat_instance <- "1"
          blank_row$redcap_repeat_instrument <- change_form
        }
        blank_row[[field_name_to_change]] <- NA
      } else {
        print(form_change)
      }
      choice1 <- utils::menu(choices1, title = paste0("What would you like to do?"))
      if (choice1 == 3) {
        project %>% link_REDCap_record(record = record)
      }
      if (choice1 == 2) {
        if (nrow(form_change) == 0) form_change <- blank_row
        for (j in seq_len(nrow(form_change))) {
          message("Old answer (", field_name_to_change, "): ", form_change[j, field_name_to_change])
          choice2 <- utils::menu(choices2, title = paste0("What would you like to do?"))
          choice <- choices2[choice2]
          form_sub <- form_change[j, ]
          if (choice %in% c("Manual Entry", "Do Nothing", "Launch Redcap Link Only")) {
            if (choice == "Do Nothing") {
              message("Did not change anything")
            }
            if (choice == "Manual Entry") {
              form_sub[[field_name_to_change]] <- readline("What would you like it to be? ")
              if (upload_individually) {
                form_sub %>%
                  labelled_to_raw_form(project) %>%
                  upload_form_to_REDCap(project)
                message("Uploaded: ", form_sub %>% paste0(collapse = " | "))
                record_was_updated <- TRUE
              } else {
                form <- form %>% dplyr::bind_rows(form_sub)
              }
            }
            if (choice == "Launch Redcap Link Only") { # account for repeat? instance
              project %>% link_REDCap_record(record = record, page = change_form, instance = form_change[j, "redcap_repeat_instance"])
            }
          } else {
            form_sub[[field_name_to_change]] <- choice
            if (upload_individually) {
              form_sub %>%
                labelled_to_raw_form(project) %>%
                upload_form_to_REDCap(project)
              message("Uploaded: ", form_sub %>% paste0(collapse = " | "))
              record_was_updated <- TRUE
            } else {
              form <- form %>% dplyr::bind_rows(form_sub)
            }
          }
        }
        if (is_repeating_form) {
          choice3 <- 2
          the_max <- 0
          if (nrow(form_change) > 0) {
            the_max <- form_change$redcap_repeat_instance %>%
              as.integer() %>%
              max()
          }
          while (choice3 == 2) {
            choice3 <- utils::menu(c("No", "Yes"), title = paste0("Would you like to add an additional instance?"))
            if (choice3 == 2) {
              form_sub <- data.frame(
                record_id = record,
                redcap_repeat_instrument = change_form,
                redcap_repeat_instance = as.character(the_max + 1)
              )
              colnames(form_sub)[1] <- project$redcap$id_col
              choice2 <- utils::menu(choices2, title = paste0("What would you like to do?"))
              choice <- choices2[choice2]
              if (choice %in% c("Manual Entry", "Do Nothing", "Launch Redcap Link Only")) {
                if (choice == "Do Nothing") {
                  message("Did not change anything")
                }
                if (choice == "Manual Entry") {
                  form_sub[[field_name_to_change]] <- readline("What would you like it to be? ")
                  if (upload_individually) {
                    form_sub %>%
                      labelled_to_raw_form(project) %>%
                      upload_form_to_REDCap(project)
                    message("Uploaded: ", form_sub %>% paste0(collapse = " | "))
                    record_was_updated <- TRUE
                  } else {
                    form <- form %>% dplyr::bind_rows(form_sub)
                  }
                  the_max <- the_max + 1
                }
                if (choice == "Launch Redcap Link Only") { # account for repeat? instance
                  project %>% link_REDCap_record(record = record, page = change_form, instance = form_change[j, "redcap_repeat_instance"])
                }
              } else {
                form_sub[[field_name_to_change]] <- choice
                if (upload_individually) {
                  form_sub %>%
                    labelled_to_raw_form(project) %>%
                    upload_form_to_REDCap(project)
                  message("Uploaded: ", form_sub %>% paste0(collapse = " | "))
                  record_was_updated <- TRUE
                } else {
                  form <- form %>% dplyr::bind_rows(form_sub)
                }
                the_max <- the_max + 1
              }
            }
          }
        }
      }
    }
    if (record_was_updated) project <- sync_project(project)
  }
  if (!upload_individually) {
    form %>%
      labelled_to_raw_form(project) %>%
      upload_form_to_REDCap(project)
  }
}
