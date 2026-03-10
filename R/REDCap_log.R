#' @noRd
clean_redcap_log <- function(redcap_log, drop_exports = FALSE) {
  redcap_log <- unique(redcap_log)
  redcap_log$record_id <- NA
  redcap_log$action_type <- NA
  redcap_log <- redcap_log |>
    lapply(function(x) {
      trimws(x, whitespace = .whitespace)
    }) |>
    as.data.frame()
  design_test <- redcap_log$action == "Manage/Design"
  design_rows <- which(design_test)
  not_design_rows <- which(!design_test)
  # notdesign action -----
  keep <- starts_with(match = .log_action_records,
                      vars = redcap_log$action[not_design_rows])
  record_rows <- not_design_rows[keep]
  redcap_log$record_id[record_rows] <- gsub(
    paste0(
      "Update record",
      "|",
      "Delete record",
      "|",
      "Create record",
      "|",
      "[:(:]API[:):]",
      "|",
      "Auto",
      "|",
      "calculation",
      "|",
      "Lock/Unlock Record ",
      "|",
      " ",
      "|",
      "[:):]",
      "|",
      "[:(:]"
    ),
    "",
    redcap_log$action[record_rows]
  )
  redcap_log$action_type[record_rows] <- redcap_log$action[record_rows] |>
    strsplit(" ") |>
    lapply(function(x) {
      x[[1L]]
    }) |>
    unlist()
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = not_design_rows,
                                   action_var = "action",
                                   start_match = .log_action_exports,
                                   label = "Exports")
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = not_design_rows,
                                   action_var = "action",
                                   start_match = .log_action_users,
                                   label = "Users")
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = not_design_rows,
                                   action_var = "action",
                                   start_match = .log_action_no_changes,
                                   label = "No Changes")
  # design details  -------------------
  keep <- starts_with(match = .log_details_comments,
                      vars = redcap_log$details[design_rows])
  comment_rows <- design_rows[keep]
  extracted_ids <- str_extract(string = redcap_log$details[comment_rows],
                               pattern = "(?<=Record: )[^,]+")
  redcap_log$record_id[comment_rows] <- extracted_ids
  redcap_log$action_type[comment_rows] <- "Comment"
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = design_rows,
                                   action_var = "details",
                                   start_match = .log_details_exports,
                                   label = "Exports")
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = design_rows,
                                   action_var = "details",
                                   start_match = .log_details_metadata_major,
                                   label = "Metadata Change Major")
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = design_rows,
                                   action_var = "details",
                                   start_match = .log_details_metadata_minor,
                                   label = "Metadata Change Minor")
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = design_rows,
                                   action_var = "details",
                                   start_match = .log_details_no_changes,
                                   label = "No Changes")
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = design_rows,
                                   action_var = "details",
                                   start_match = .log_details_tokens,
                                   label = "Tokens")
  redcap_log <- redcap_log_labeler(redcap_log = redcap_log,
                                   compare_rows = design_rows,
                                   action_var = "details",
                                   start_match = .log_details_repository,
                                   label = "Repository")
  # end ------------
  row_index <- which(is.na(redcap_log$record) &
                       !is.na(redcap_log$record_id))
  redcap_log$record[row_index] <- redcap_log$record_id[row_index]
  row_index <- which(!is.na(redcap_log$record) &
                       is.na(redcap_log$record_id))
  redcap_log$action_type[row_index] <- "Users"
  redcap_log$record_id <- NULL
  redcap_log$username[which(redcap_log$username == "[survey respondent]")] <- NA
  cannot_label_rows <- which(is.na(redcap_log$action_type))
  if (length(cannot_label_rows) > 0L) {
    warning_about_unlabelled_log <- paste0(
      "Some log elements could not be labelled... Report to issues page \n",
      "    `x <- project$redcap$log[which(is.na(redcap_log$action_type)), ]`"
    )
    cli_alert_warning(warning_about_unlabelled_log)
  }
  if (drop_exports) {
    redcap_log <- redcap_log[which(redcap_log$action_type != "Exports"), ]
  }
  if (nrow(redcap_log) == 0L) {
    return(redcap_log)
  }
  new_order <- order(redcap_log$timestamp, decreasing = TRUE)
  redcap_log <- unique(redcap_log[new_order, ])
  redcap_log
}
#' @noRd
redcap_log_labeler <- function(redcap_log,
                               compare_rows,
                               action_var,
                               start_match,
                               label) {
  start_vars <- redcap_log[[action_var]][compare_rows]
  keep <- starts_with(match = start_match, vars = start_vars)
  redcap_log$action_type[compare_rows[keep]] <- label
  redcap_log
}
#' @noRd
.log_action_exports <- c("Data export", "Download uploaded ")
#' @noRd
.log_details_exports <- c("Download ", "Export ")
#' @noRd
.log_action_users <- c("Add user ",
                       "Create user role",
                       "Delete user ",
                       "Edit user ",
                       "Rename user role",
                       "User assigned to role ",
                       "User removed from user role")
#' @noRd
.log_details_comments <- c("Add field comment ",
                           "Edit field comment ",
                           "Delete field comment ")
#' @noRd
.log_action_records <- c("Create record ",
                         "Delete record ",
                         "Lock/Unlock Record ",
                         "Update record ")
#' @noRd
.log_action_no_changes <- c("Enable external module ",
                            "Disable external module ",
                            "Modify configuration for external module ",
                            "Update Response")
#' @noRd
.log_details_no_changes <- c("Add settings for automated survey invitations",
                             "Add/edit stop actions for survey",
                             "Approve production project modifications",
                             "Automatically schedule survey invitation",
                             "Cancel draft mode",
                             "CDIS settings updated",
                             "Checked off item in project checklist",
                             "Click project bookmark",
                             "Copy report",
                             "Create custom record dashboard",
                             "Create data access group",
                             "Create project bookmark",
                             "Create project dashboard",
                             "Create report",
                             "Delete custom record dashboard",
                             "Delete data access group",
                             "Delete project bookmark",
                             "Delete project dashboard",
                             "Delete report",
                             "Disable auto variable",
                             "Disable Google reCAPTCHA",
                             "Disabled survey notification for user",
                             "Edit project bookmark",
                             "Edit project dashboard",
                             "Edit report",
                             "Edit settings for automated survey invitation",
                             "Edit settings for Form Render Skip Logic",
                             "Edit settings for survey queue",
                             "Email survey link",
                             "Enable auto variable",
                             "Enable Clinical Data Pull (CDP) module",
                             "Enable Google reCAPTCHA on public survey",
                             "Enabled survey notification",
                             "Enter draft mode",
                             "Entered Draft Preview",
                             "Execute data quality rule",
                             "Exited Draft Preview mode",
                             "Mapping configuration created",
                             "Mapping configuration deleted",
                             "Mapping configuration updated",
                             "Message added to the queue",
                             "Modify custom record dashboard",
                             "Multi-Language Management",
                             "Reject production proj",
                             "Reorder project bookmarks",
                             "Reorder report",
                             "Request approval for",
                             "Send email ",
                             "Send file ",
                             "Send request to copy project",
                             "Send request to move project",
                             "Send survey invitation",
                             "Switch DAG ",
                             "Unchecked item in project checklist",
                             "updated non-adjudicated item_count",
                             "User opted in to access")
#' @noRd
.log_details_tokens <- c("Create API token",
                         "Request API token",
                         "User delete own API token",
                         "User regenerate own API token")
#' @noRd
.log_details_repository <- c("Create folder in File Repository",
                             "Delete file from File Repository",
                             "Delete folder from File Repository",
                             "Upload document to file repository",
                             "Upload file to File Repository")
#' @noRd
.log_details_metadata_minor <- c("Add/edit branching logic",
                                 "Delete section header",
                                 "Modify survey title",
                                 "Move project field",
                                 "Reorder data collection instruments",
                                 "Reorder events",
                                 "Reorder project fields",
                                 "Tag new identifier fields")
#' @noRd
.log_details_metadata_major <- c("Copy data collection instrument",
                                 "Copy project ",
                                 "Create arm",
                                 "Create data collection instrument",
                                 "Create event",
                                 "Create matrix of fields",
                                 "Create project",
                                 "Create project field",
                                 "Delete arm",
                                 "Delete data collection instrument",
                                 "Delete event",
                                 "Delete matrix of fields",
                                 "Delete project field",
                                 "Delete survey",
                                 "Download instrument from Shared Library",
                                 "Edit arm",
                                 "Edit event",
                                 "Edit project field",
                                 "Erase all data",
                                 "Make project customizations",
                                 "Modify project settings",
                                 "Modify survey info",
                                 "Move project ",
                                 "Perform instrument-event mappings",
                                 "Rename data collection instrument",
                                 "Set up repeating instruments",
                                 "Set up survey",
                                 "Upload data dictionary",
                                 "Upload document for image")
