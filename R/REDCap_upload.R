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
#' issued. The function goes through each table in the new data and compares it
#' with the old data, recording the differences.
#'
#' The `compare` and `to` parameters allow users to specify specific data
#' choices to compare, though their exact usage will depend on how the function
#' is fully implemented.
#' @export
find_upload_diff <- function(to_be_uploaded,
                             project,
                             view_old = FALSE,
                             n_row_view = 20) {
  project <- assert_blank_project(project)
  # if (!all(names(new_list) %in% project$metadata$forms$form_name)) warning("All upload names should ideally match the project form names, `project$metadata$forms$form_name`", immediate. = TRUE)
  # already_used <- NULL
  was_df <- is.data.frame(to_be_uploaded)
  was_list <- is.list(to_be_uploaded)
  if (!was_df && !was_list) {
    stop("`to_be_uploaded` must be list of data.frames or a date.frame")
  }
  if (was_df) {
    to_be_uploaded <- list(upload_me = to_be_uploaded)
  }
  for (user_name in names(to_be_uploaded)) { # form_name <- names(new_list) %>% sample(1)
    new <- to_be_uploaded[[user_name]]
    ref_cols <- project$metadata$raw_structure_cols
    ref_cols <- ref_cols[which(ref_cols %in% colnames(new))]
    data_cols <- colnames(new)[which(!colnames(new) %in% ref_cols)]
    form_names <- field_names_to_form_names(project, data_cols)
    # if (any(form_names %in% already_used)) {
    #   stop("REDCapSync will not allow you to upload items from same form multiple times in one loop without refreshing.")
    # }
    if (length(form_names) > 1) {
      if (!all(form_names %in% project$metadata$forms$form_name[which(project$metadata$forms$repeating)])) {
        stop("Can't have variables in multiple forms in an upload data.frame unless it is a non-repeating form")
      }
      stop("Can only upload data from one form at a time.")
    }
    relevant_data_cols <- data_cols %>% vec1_in_vec2(
      form_names_to_field_names(
        form_names = form_names,
        project = project
      )
    )
    keep <- c(ref_cols, relevant_data_cols)
    drop <- data_cols %>% vec1_not_in_vec2(form_names_to_field_names(form_names = form_names, project = project))
    if (length(drop) > 0) {
      message("Dropping field_names that aren't part of REDCap metadata: ", toString(drop))
    }
    final <- find_form_diff2(
      new = new[, keep],
      old = project$data[[form_names]][, keep],
      ref_cols = ref_cols,
      message_pass = paste0(user_name, ": "),
      view_old = view_old,
      n_row_view = n_row_view
    )
    if (was_df) {
      to_be_uploaded <- final
    } else {
      to_be_uploaded[[user_name]] <- final
    }
  }
  if (is_something(to_be_uploaded)) {
    return(invisible(to_be_uploaded))
  }
  message("No upload updates!")
  invisible(NULL)
}
