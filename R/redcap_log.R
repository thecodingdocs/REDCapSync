#' @noRd
clean_redcap_log <- function(redcap_log, drop_exports = FALSE) {
  redcap_log <- unique(redcap_log)
  redcap_log$record_id <- NA
  redcap_log$action_type <- NA
  redcap_log <- redcap_log |>
    lapply(function(x) {
      trimws(x, whitespace = WHITESPACE)
    }) |>
    as.data.frame()
  design_test <- redcap_log$action == "Manage/Design"
  design_rows <- which(design_test)
  not_design_rows <- which(!design_test)
  # notdesign action -----
  keep <- starts_with(match = LOG_ACTION_RECORDS,
                      vars = redcap_log$action[not_design_rows])
  record_rows <- not_design_rows[keep]
  redcap_log$record_id[record_rows] <- redcap_log$record[record_rows]
  redcap_log$action_type[record_rows] <- redcap_log$action[record_rows] |>
    strsplit(" ") |>
    lapply(function(x) {
      x[[1L]]
    }) |>
    unlist()
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = not_design_rows,
                                    action_var = "action",
                                    start_match = LOG_ACTION_EXPORTS,
                                    label = "Exports")
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = not_design_rows,
                                    action_var = "action",
                                    start_match = LOG_ACTION_USERS,
                                    label = "Users")
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = not_design_rows,
                                    action_var = "action",
                                    start_match = LOG_ACTION_NO_CHANGES,
                                    label = "No Changes")
  # design details  -------------------
  keep <- starts_with(match = LOG_DETAILS_COMMENTS,
                      vars = redcap_log$details[design_rows])
  comment_rows <- design_rows[keep]
  extracted_ids <- str_extract(string = redcap_log$details[comment_rows],
                               pattern = "(?<=Record: )[^,]+")
  redcap_log$record_id[comment_rows] <- extracted_ids
  redcap_log$action_type[comment_rows] <- "Comment"
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = design_rows,
                                    action_var = "details",
                                    start_match = LOG_DETAILS_EXPORTS,
                                    label = "Exports")
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = design_rows,
                                    action_var = "details",
                                    start_match = LOG_DETAILS_METADATA_MAJOR,
                                    label = "Metadata Change Major")
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = design_rows,
                                    action_var = "details",
                                    start_match = LOG_DETAILS_METADATA_MINOR,
                                    label = "Metadata Change Minor")
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = design_rows,
                                    action_var = "details",
                                    start_match = LOG_DETAILS_NO_CHANGES,
                                    label = "No Changes")
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = design_rows,
                                    action_var = "details",
                                    start_match = LOG_DETAILS_TOKENS,
                                    label = "Tokens")
  redcap_log <- redcap_log_labeller(redcap_log = redcap_log,
                                    compare_rows = design_rows,
                                    action_var = "details",
                                    start_match = LOG_DETAILS_REPOSITORY,
                                    label = "Repository")
  # end ------------
  redcap_log$record_id <- NULL
  ignore <- which(redcap_log$action_type == "Update" & is.na(redcap_log$record))
  redcap_log$action_type[ignore] <- "No Changes"
  ignore <- which(is.na(redcap_log$action_type) &
                    (is.na(redcap_log$record) | redcap_log$record == "") &
                    (is.na(redcap_log$details) | redcap_log$details == "") &
                    (is.na(redcap_log$action) | redcap_log$action == ""))
  redcap_log$action_type[ignore] <- "No Changes"
  redcap_log$username[which(redcap_log$username == "[survey respondent]")] <- NA
  cannot_label_rows <- which(is.na(redcap_log$action_type))
  if (length(cannot_label_rows) > 0L) {
    warning_about_unlabelled_log <- paste0(
      "Some log elements could not be labelled... Report to issues page \n",
      "`project$redcap$log[which(is.na(project$redcap$log$action_type)), ]`"
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
  rownames(redcap_log) <- NULL
  redcap_log
}
#' @noRd
redcap_log_labeller <- function(redcap_log,
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
get_interim_log <- function(project) {
  interim_log <- NULL
  if (!project$redcap$has_log_access) {
    cli_alert_danger(
      paste0( # fix for when access is changed
        "You do not have logging access to this REDCap project. If you ",
        "want to refresh the data use `project$sync(hard_reset = TRUE)`.",
        " Request logging access for more effecient use of API exports."
      )
    )
    return(interim_log)
  }
  interim_log <- get_redcap_log(project, log_begin_date = as.Date(
    strptime(project$redcap$log$timestamp[1L], format = "%Y-%m-%d")
  )) |> unique()
  if (nrow(interim_log) <= nrow(project$redcap$log)) {
    head_of_log <- head(project$redcap$log, n = nrow(interim_log))
  } else {
    head_of_log <- project$redcap$log
  }
  unique_log <- interim_log |>
    bind_rows(head_of_log) |>
    unique()
  # dup <- unique_log[
  #which(duplicated(
  #rbind(unique_log, head_of_log),fromLast = TRUE)[
  #seq_len(nrow(unique_log)]), ]
  log_to_clean <- bind_rows(unique_log, head_of_log)
  indices <- seq_len(nrow(unique_log))
  unique_rows <- which(!duplicated(log_to_clean, fromLast = TRUE)[indices])
  interim_log <- unique_log[unique_rows, ]
  interim_log
}
#' @noRd
analyze_log <- function(interim_log, project) {
  log_changes <- list(
    hard_reset = FALSE,
    refresh_metadata = FALSE,
    refresh_metadata_major = FALSE,
    refresh_metadata_minor = FALSE,
    refresh_users = FALSE,
    refresh_data = FALSE,
    deleted_records = NULL,
    updated_records = NULL,
    renamed_records = NULL,
    comment_records = NULL,
    length_deleted_records = 0L,
    length_updated_records = 0L,
    length_renamed_records = 0L,
    length_comment_records = 0L
  )
  if (nrow(interim_log) > 0L) {
    # assert_log?
    # interim_log timestamp ? NULL
    maybe_metadata <- interim_log$action_type[which(is.na(interim_log$record))]
    # inclusion
    refresh_metadata_major <- "Metadata Change Major" %in% maybe_metadata
    refresh_metadata_minor <- "Metadata Change Minor" %in% maybe_metadata
    log_changes$refresh_metadata_major <- refresh_metadata_major
    log_changes$refresh_metadata_minor <- refresh_metadata_minor
    log_changes$refresh_users <- "Users" %in% interim_log$action_type
    log_changes$refresh_metadata <- refresh_metadata_major ||
      refresh_metadata_minor
    if (log_changes$refresh_metadata) {
      # account for minor changes later
      log_changes$hard_reset <- TRUE
      log_changes$refresh_data <- TRUE # this can change with more code
      log_changes$refresh_users <- TRUE
      return(log_changes)
    }
    interim_log_data <- interim_log[which(!is.na(interim_log$record)), ]
    interim_log_data <-
      interim_log_data[which(interim_log_data$action_type != "Users"), ]
    if (nrow(interim_log_data) > 0L) {
      log_list <- split(interim_log_data, interim_log_data$action_type)
      log_changes$comment_records <- unique(log_list$Comment$record)
      log_changes$deleted_records <- unique(log_list$Delete$record)
      created_records <- unique(log_list$Create$record)
      log_changes$updated_records <- log_changes$deleted_records |>
        append(created_records) |>
        append(log_list$Update$record) |>
        unique()
      if (length(log_list$Update$details) > 0L) {
        keep_updates <- log_list$Update$details |>
          str_replace_all(" = '[^']*'", "") |>
          strsplit(", ") |>
          lapply(function(detail) {
            project$metadata$id_col %in% detail
          }) |>
          unlist() |>
          which()
        renamed_records <- log_list$Update$record[keep_updates] |> unique()
        if (length(renamed_records) > 0L) {
          log_changes$renamed_records <-  renamed_records
        }
      }
      log_changes$length_deleted_records <- length(log_changes$deleted_records)
      log_changes$length_updated_records <- length(log_changes$updated_records)
      log_changes$length_renamed_records <- length(log_changes$renamed_records)
      log_changes$length_comment_records <- length(log_changes$comment_records)
      log_changes$refresh_data <- (log_changes$length_deleted_records > 0L) ||
        (log_changes$length_updated_records > 0L) ||
        (log_changes$length_renamed_records > 0L)
    }
  }
  log_changes
}
#' @noRd
log_change_messages <- function(log_changes, max_print = 8L) {
  if (log_changes$refresh_metadata) {
    cli_alert_warning("Full update triggered: Metadata was changed!")
    return(invisible())
  }
  nothing_to_do <- sum(
    log_changes$length_deleted_records,
    log_changes$length_updated_records,
    log_changes$length_renamed_records,
    log_changes$length_comment_records
  ) == 0L
  if (nothing_to_do) {
    cli_alert_success("Up to date already!")
    return(invisible())
  }
  if (log_changes$length_deleted_records > 0L) {
    cli_alert_info(paste(
      "Deleted:",
      ifelse(
        log_changes$length_deleted_records > max_print,
        paste(log_changes$length_deleted_records, "records"),
        toString(log_changes$deleted_records)
      )
    ))
  }
  if (log_changes$length_updated_records > 0L) {
    cli_alert_info(paste0(
      "Updated: ",
      ifelse(
        log_changes$length_updated_records > max_print,
        paste(log_changes$length_updated_records, "records"),
        toString(log_changes$updated_records)
      )
    ))
  }
  if (log_changes$length_renamed_records > 0L) {
    cli_alert_info(paste(
      "Possibly Renamed:",
      ifelse(
        log_changes$length_renamed_records > max_print,
        paste(log_changes$length_renamed_records, "records"),
        toString(log_changes$renamed_records)
      )
    ))
  }
  if (log_changes$length_comment_records > 0L) {
    cli_alert_info(paste(
      "Comments:",
      ifelse(
        log_changes$length_comment_records > max_print,
        paste(log_changes$length_comment_records, " records"),
        toString(log_changes$comment_records)
      )
    ))
  }
  invisible(NULL)
}
#' @noRd
check_redcap_former_names <- function(record = NULL, project) {
  renamed_log <- NULL
  if (!is.null(get_redcap_log)) {
    creation_time <- project$redcap$project_info$creation_time
    log_begin_date <- as.Date(as.POSIXct(creation_time)) - 1L
    renamed_log <- get_redcap_log(project = project,
                                  log_begin_date = log_begin_date,
                                  record = record)
    # consider dropping anything prior to a delete
  }
  renamed_log
}
#' @noRd
extract_log_all_names <- function(renamed_log, project) {
  pattern <- paste0(project$metadata$id_col, "\\s*=\\s*'([^']+)'")
  renamed_log$record |>
    append(str_match(renamed_log$details, pattern = pattern)[, 2]) |>
    unique() |>
    drop_nas()
}
#' @noRd
get_redcap_log_update <- function(records = NULL, project) {
  output <- list(records = NULL, log = NULL)
  while (length(records) > 0L) {
    record <- records[1L]
    records <- records[-1L]
    output$records <- output$records |> append(record)
    renamed_log <- record |> check_redcap_former_names(project)
    if (nrow(renamed_log) > 0L) {
      other_names <- renamed_log |>
        extract_log_all_names(project) |>
        drop_nas() |>
        unique() |>
        setdiff(output$records) |>
        setdiff(records)
      output$log <- output$log |> bind_rows(renamed_log)
      if (length(other_names) > 0L) {
        records <- records |> append(other_names)
      }
    }
  }
  if (is_something(output$log)) {
    new_order <- order(output$log$timestamp, decreasing = TRUE)
    output$log <- unique(output$log[new_order, ])
    rownames(output$log) <- NULL
  }
  output
}
#' @noRd
LOG_ACTION_EXPORTS <- c("Data export", "Download uploaded ", "PDF Export")
#' @noRd
LOG_DETAILS_EXPORTS <- c("Download ", "Export ")
#' @noRd
LOG_ACTION_USERS <- c("Add user ",
                      "Create user role",
                      "Delete user ",
                      "Edit user ",
                      "Rename user role",
                      "User assigned to role ",
                      "User removed from user role",
                      "Updated User Expiration ",
                      "Update user ")
#' @noRd
LOG_DETAILS_COMMENTS <- c("Add field comment ",
                          "Edit field comment ",
                          "Delete field comment ")
#' @noRd
LOG_ACTION_RECORDS <- c("Create record ",
                        "Create Response ",
                        "Delete record ",
                        "Deleted Document ", # should look at files
                        "Lock/Unlock Record ",
                        "Uploaded Document ",
                        "Update record ",
                        "Update Response")
#' @noRd
LOG_ACTION_NO_CHANGES <- c("Enable external module ",
                           "Disable external module ",
                           "Lock/Unlock Record ", # should not need to refresh
                           "Modify configuration for external module ")
#' @noRd
LOG_DETAILS_NO_CHANGES <- c("Add settings for automated survey invitations",
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
LOG_DETAILS_TOKENS <- c("Create API token",
                        "Request API token",
                        "User delete own API token",
                        "User regenerate own API token")
#' @noRd
LOG_DETAILS_REPOSITORY <- c("Create folder in File Repository",
                            "Delete file from File Repository",
                            "Delete folder from File Repository",
                            "Upload document to file repository",
                            "Upload file to File Repository")
#' @noRd
LOG_DETAILS_METADATA_MINOR <- c("Add/edit branching logic",
                                "Delete section header",
                                "Modify survey title",
                                "Move project field",
                                "Reorder data collection instruments",
                                "Reorder events",
                                "Reorder project fields",
                                "Tag new identifier fields")
#' @noRd
LOG_DETAILS_METADATA_MAJOR <- c("Copy data collection instrument",
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
