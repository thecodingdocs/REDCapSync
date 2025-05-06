#' @title Synchronize REDCap Data
#' @description
#' Updates the REDCap database (`project` object) by fetching the latest data
#' from the REDCap server.
#'
#' @details
#' This function updates the REDCap database by fetching the latest data from
#' the REDCap server. It supports various options such as forcing a fresh
#' update, checking logs for a specified number of days, and retrieving files
#' from REDCap. The function can also handle metadata-only updates and batch
#' processing.
#'
#' @inheritParams save_project
#' @param reset Logical that forces a fresh update if TRUE. Default is `FALSE`.
#' @param summarize Logical (TRUE/FALSE). If TRUE, summarizes data to directory.
#' @param save_to_dir Logical (TRUE/FALSE). If TRUE, saves the updated data to
#' the directory. Default is `TRUE`.
#' @return Messages for confirmation.
#' @seealso
#' \link{setup_project} for initializing the `project` object.
#' @family db_functions
#' @export
sync_project <- function(
    project,
    reset = FALSE,
    silent = FALSE,
    summarize = TRUE,
    save_to_dir = TRUE) {
  collected <- makeAssertCollection()
  assert_blank_project(project)
  assert_logical(reset, any.missing = FALSE, len = 1, add = collected)
  # assert_logical(
  #   ask_about_overwrites,
  #   any.missing = FALSE,
  #   len = 1,
  #   add = collected
  # )
  assert_logical(save_to_dir, any.missing = FALSE, len = 1, add = collected)
  assert_logical(silent, any.missing = FALSE, len = 1, add = collected)
  current_function <- as.character(current_call())[[1]]
  if (!collected$isEmpty()) {
    message <- collected %>% cli_message_maker(function_name = current_function)
    cli::cli_abort(message)
  }
  do_it <- due_for_sync(project_name = project$short_name) || reset
  was_updated <- FALSE
  if (!do_it) {
    cli::cli_alert_info("{project$short_name} not due for sync ({project$internals$sync_frequency})")
  }
  if(do_it){
    stale_records <- NULL
    will_update <- TRUE
    # project$internals$last_directory_save
    # project$internals$last_test_connection_attempt
    project <- test_project_token(project)
    connected <- project$internals$last_test_connection_outcome
    if (!connected) {
      cli_alert_danger("Could not connect to REDCap")
      return(invisible(project))
    }
    # project$internals$last_metadata_update <-  now_time()-lubridate::days(1)
    # project$internals$last_data_update <-  now_time()-lubridate::days(1)
    if (is_something(project$transformation$data_updates)) {
      cli_alert_wrap(
        "There is data in 'project$transformation$data_updates' that has not been pushed to REDCap yet..."
      )
      # choice <- utils::menu(
      #   choices = c("Yes", "No and skip", "No and stop the function!"),
      #   title = "Would you like to review these updates now?"
      # )
      # if (choice == 3){
      #   stop("Stopped as requested!")
      # }
      # if (choice == 1) {
      #   project <- upload_project_to_REDCap(project, ask = TRUE)
      # }
    }
    if (!reset) { # check log interim
      if (
        !is_something(project$internals$last_metadata_update) ||
        !is_something(project$internals$last_data_update) ||
        !is_something(project$internals$last_full_update)
      ) {
        reset <- TRUE
      } else {
        interim_log <- get_REDCap_log(project, log_begin_date = as.Date(strptime(project$redcap$log$timestamp[1], format = "%Y-%m-%d"))) %>% unique()
        if (nrow(interim_log) <= nrow(project$redcap$log)) {
          head_of_log <- project$redcap$log %>% utils::head(n = nrow(interim_log))
        } else {
          head_of_log <- project$redcap$log
        }
        unique_log <- interim_log %>%
          rbind(head_of_log) %>%
          unique()
        # dup <- unique_log[which(duplicated(rbind(unique_log, head_of_log),fromLast = TRUE)[seq_len(nrow(unique_log)]), ]
        interim_log <- unique_log[which(!duplicated(rbind(unique_log, head_of_log), fromLast = TRUE)[seq_len(nrow(unique_log))]), ]
        if (nrow(interim_log) > 0) {
          project$redcap$log <- interim_log %>%
            dplyr::bind_rows(project$redcap$log) %>%
            unique()
          interim_log$timestamp <- NULL
          interim_log_metadata <- interim_log[which(is.na(interim_log$record)), ]
          interim_log_metadata <- interim_log_metadata[which(interim_log_metadata$action_type == "Metadata Change Major"), ] # inclusion
          interim_log_metadata_minor <- any(interim_log_metadata$action_type == "Metadata Change Minor")
          # interim_log_metadata <- interim_log_metadata[grep(ignore_redcap_log(),interim_log_metadata$details,ignore.case = TRUE,invert = TRUE) %>% unique(),]
          if (nrow(interim_log_metadata) > 0 || interim_log_metadata_minor) { # account for minor changes later
            reset <- TRUE
            message("Update because: Metadata was changed!")
          } else {
            interim_log_data <- interim_log[which(!is.na(interim_log$record)), ]
            interim_log_data <- interim_log_data[which(interim_log_data$action_type != "Users"), ]
            deleted_records <- interim_log_data$record[which(interim_log_data$action_type == "Delete")]
            if (length(deleted_records) > 0) {
              warning(
                "There were recent records deleted from redcap Consider running",
                " with 'reset = TRUE'. Records: ",
                deleted_records %>% toString(),
                immediate. = TRUE
              )
            }
            stale_records <- interim_log_data$record %>% unique()
            if (length(stale_records) == 0) {
              stale_records <- NULL
              will_update <- FALSE
            }
          }
        } else {
          will_update <- FALSE
        }
      }
    }
    if (reset) {
      project <- project %>% get_REDCap_metadata(include_users = !project$internals$metadata_only)
      # project$internals$is_transformed <- FALSE
      if (!project$internals$metadata_only) {
        project$data <- list()
        project$data_updates <- list()
        project$summary <- list()
        project$data <- project %>% get_REDCap_data(
          labelled = project$internals$labelled,
          batch_size = project$internals$batch_size_download
        )
        log <- project$redcap$log # in case there is a log already
        if (project$internals$entire_log) {
          log_begin_date <- as.POSIXct(project$redcap$project_info$creation_time) %>% as.Date()
        } else {
          log_begin_date <- Sys.Date() - project$internals$days_of_log
        }
        project$redcap$log <- log %>%
          dplyr::bind_rows(
            project %>% get_REDCap_log(log_begin_date = log_begin_date)
          ) %>%
          sort_redcap_log()
        # project <- annotate_fields(project)
        # project <- annotate_choices(project)
        project$summary$all_records <- extract_project_records(project)
        project$summary$all_records$last_api_call <-
          project$internals$last_full_update <-
          project$internals$last_metadata_update <-
          project$internals$last_data_update <- now_time()
        cli_alert_wrap(paste0("Full ", project$short_name, " update!"), bullet_type = "v")
        was_updated <- TRUE
      }
    } else {
      # if(interim_log_metadata_minor){
      #   # what if transformed already
      #   # if(project$internals$is_transformed){
      #   #   # untransform
      #   # }
      #   project <- project %>% get_REDCap_metadata(include_users = !metadata_only)
      # }
      if (will_update) {
        project$data <- project$data %>% all_character_cols_list()
        if (length(deleted_records) > 0) {
          stale_records <- stale_records[which(!stale_records %in% deleted_records)]
          project <- remove_records_from_project(project = project, records = deleted_records)
        }
        form_list <- project %>% get_REDCap_data(labelled = project$internals$labelled, records = stale_records)
        missing_from_summary <- stale_records[which(!stale_records %in% project$summary$all_records[[project$redcap$id_col]])]
        if (length(missing_from_summary) > 0) {
          x <- data.frame(
            record = missing_from_summary,
            last_api_call = NA,
            was_tranformed = FALSE,
            was_saved = FALSE,
            stringsAsFactors = FALSE
          )
          colnames(x)[1] <- project$redcap$id_col
          project$summary$all_records <- project$summary$all_records %>% dplyr::bind_rows(x)
          project$summary$all_records <- project$summary$all_records[order(project$summary$all_records[[project$redcap$id_col]], decreasing = TRUE), ]
        }
        project$summary$all_records$last_api_call[which(project$summary$all_records[[project$redcap$id_col]] %in% stale_records)] <-
          project$internals$last_data_update <-
          now_time()
        project <- remove_records_from_project(project = project, records = stale_records)
        if (!all(names(form_list) %in% names(project$data))) {
          stop("Imported data names doesn't match project$data names. If this happens run `sync_project(project, reset = TRUE)`")
        }
        for (form_name in names(form_list)) {
          project$data[[form_name]] <- project$data[[form_name]] %>%
            all_character_cols() %>%
            dplyr::bind_rows(form_list[[form_name]])
        }
        message("Updated: ", toString(stale_records))
        was_updated <- TRUE
      } else {
        message("Up to date already!")
      }
    }
    if (project$internals$get_files) { # test now
      get_REDCap_files(
        project,
        original_file_names = project$internals$original_file_names
      )
    }
    project$internals$last_sync <- now_time()
  }
  if (save_to_dir && !is.null(project$dir_path)) {
    first_stamp <- project$internals$last_data_transformation
    project <- transform_project(project)
    second_stamp <- project$internals$last_data_transformation
    was_updated <- was_updated || !identical(first_stamp,second_stamp)
    if (summarize) {
      first_stamp <- project$internals$last_data_transformation
      project <- summarize_project(
        project = project,
        reset = reset
      )
      second_stamp <- project$internals$last_data_transformation
    }
    if (was_updated) {
      project <- save_project(project)
    } else {
      project$internals$last_directory_save <- project$internals$last_sync
      save_project_details(project)
    }
  }
  invisible(project)
}
