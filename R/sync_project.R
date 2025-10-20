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
#' @param summarize Logical (TRUE/FALSE). If TRUE, summarizes data to directory.
#' @param save_to_dir Logical (TRUE/FALSE). If TRUE, saves the updated data to
#' the directory. Default is `TRUE`.
#' @param hard_check Will check REDCap even if not due (see `sync_frequency`
#' parameter from `setup_project()`)
#' @param hard_reset Logical that forces a fresh update if TRUE. Default is `FALSE`.
#' @return Messages for confirmation.
#' @seealso
#' \link{setup_project} for initializing the `project` object.
#' @family db_functions
#' @export
sync_project <- function(
    project,
    summarize = TRUE,
    save_to_dir = TRUE,
    hard_check = FALSE,
    hard_reset = FALSE,
    silent = FALSE
   ) {
  collected <- makeAssertCollection()
  assert_blank_project(project)
  assert_logical(hard_reset, any.missing = FALSE, len = 1, add = collected)
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
  id_col <- project$redcap$id_col
  do_it <- due_for_sync(project_name = project$short_name) || hard_reset || hard_check
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
    }
    if (!hard_reset) { # check log interim
      if (
        !is_something(project$internals$last_metadata_update) ||
        !is_something(project$internals$last_data_update) ||
        !is_something(project$internals$last_full_update)
      ) {
        hard_reset <- TRUE
      } else {
        interim_log <- get_REDCap_log2(project, log_begin_date = as.Date(strptime(project$redcap$log$timestamp[1], format = "%Y-%m-%d"))) %>% unique()
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
            hard_reset <- TRUE
            message("Update because: Metadata was changed!")
          } else {
            interim_log_data <- interim_log[which(!is.na(interim_log$record)), ]
            interim_log_data <- interim_log_data[which(interim_log_data$action_type != "Users"), ]
            deleted_records <- interim_log_data$record[which(interim_log_data$action_type == "Delete")]
            if (length(deleted_records) > 0) {
              warning(
                "There were recent records deleted from redcap Consider running",
                " with 'hard_reset = TRUE'. Records: ",
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
    if (hard_reset) {
      project <- project %>% get_REDCap_metadata(include_users = !project$internals$metadata_only)
      # project$internals$is_transformed <- FALSE
      # project <- clear_project_transformation(project)
      if (!project$internals$metadata_only) {
        project$data <- list()
        project$data_updates <- list()
        project$summary <- list()
        project$data <- project %>% get_REDCap_data(
          labelled = FALSE,
          batch_size = project$internals$batch_size_download
        )
        #if error records comma
        redcap_log <- project$redcap$log # in case there is a log already
        if (project$internals$entire_log) {
          log_begin_date <- as.POSIXct(project$redcap$project_info$creation_time) %>% as.Date()
        } else {
          log_begin_date <- Sys.Date() - project$internals$days_of_log
        }
        project$redcap$log <- redcap_log %>%
          dplyr::bind_rows(
            project %>% get_REDCap_log2(log_begin_date = log_begin_date)
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
          project$summary$all_records <- project$summary$all_records[which(!project$summary$all_records[[id_col]] %in% deleted_records), ]
        }
        message_string <- "No new records to update!"
        if (length(stale_records) > 0) {
          form_list <- project %>% get_REDCap_data(labelled = FALSE,
                                                   records = stale_records)
          missing_from_summary <- stale_records[which(!stale_records %in% project$summary$all_records[[id_col]])]
          if (length(missing_from_summary) > 0) {
            x <- data.frame(
              record = missing_from_summary,
              last_api_call = NA,
              was_transformed = FALSE,
              was_saved = FALSE,
              stringsAsFactors = FALSE
            )
            colnames(x)[1] <- id_col
            project$summary$all_records <- project$summary$all_records %>% dplyr::bind_rows(x)
            project$summary$all_records <- project$summary$all_records[order(project$summary$all_records[[id_col]], decreasing = TRUE), ]
          }
          project <- remove_records_from_project(project = project, records = stale_records)
          if (!all(names(form_list) %in% project$metadata$forms$form_name)) {
            stop(
              "Imported data names doesn't match project$data names. If this",
              "happens run `sync_project(project, hard_reset = TRUE)`"
            )
          }
          for (form_name in names(form_list)) {
            project$data[[form_name]] <- project$data[[form_name]] %>%
              all_character_cols() %>%
              dplyr::bind_rows(form_list[[form_name]])
          }
          row_match <- which(project$summary$all_records[[id_col]] %in% stale_records)
          project$summary$all_records$last_api_call[row_match] <-
            project$internals$last_data_update <-
            now_time()
          project$summary$all_records$was_transformed[row_match] <- FALSE
          project$summary$all_records$was_saved[row_match] <- FALSE
          message_string <- toString(stale_records)
          stale_record_length <- length(stale_records)
          if (stale_record_length > 20) {
            message_string <- stale_record_length %>% paste("records")
          }
        }
        message("Updated: ", message_string)
        was_updated <- TRUE
      } else {
        message("Up to date already!")
      }
    }
    project$internals$last_sync <- now_time()
  }
  if (project$internals$add_default_fields) {
    if(!is_something(project$transformation$fields)){
      project <- add_default_fields(project)
    }
  }
  if (project$internals$add_default_transformation) {
    if(!is_something(project$transformation$forms)){
      project <- add_default_transformation(project)
    }
  }
  if (project$internals$add_default_summaries) {
    if (!is_something(project$summary$REDCapSync) ||
        !is_something(project$summary$REDCapSync_raw)) {
      project <- add_default_summaries(project)
    }
  }
  #turn off transform for now
  # first_stamp <- project$internals$last_data_transformation
  # project <- transform_project(project)
  # second_stamp <- project$internals$last_data_transformation
  # was_updated <- was_updated || !identical(first_stamp,second_stamp)
  if (save_to_dir && !is.null(project$dir_path)) {
    if(is_something(project$data)){
      if (project$internals$get_files) { # test now
        get_REDCap_files( # would want track internally?
          project,
          original_file_names = project$internals$original_file_names
        )
      }
    }
    if (summarize) {
      first_stamp <- project$internals$last_summary
      project <- summarize_project(project = project, hard_reset = hard_reset)
      second_stamp <- project$internals$last_summary
      was_updated <- was_updated || !identical(first_stamp,second_stamp)
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
due_for_sync <- function(project_name) {
  now <- now_time()
  projects <- get_projects()
  # early escapes ----
  assert_data_frame(projects, min.rows = 1)
  assert_names(projects$short_name, must.include = project_name)
  #-----
  project_row <- which(projects$short_name == project_name)
  last_sync <- projects$last_sync[project_row]
  # assert_posixct(last_data_update, len = 1, any.missing = TRUE)
  if (is.na(last_sync)) {
    return(TRUE)
  }
  then <- last_sync
  if (is.na(then)) {
    return(TRUE)
  }
  sync_frequency <- projects$sync_frequency[project_row]
  if (sync_frequency == "always") {
    return(TRUE)
  }
  if (sync_frequency == "never") {
    return(FALSE)
  }
  have_to_check <- sync_frequency %in% c("hourly", "daily", "weekly", "monthly")
  if (have_to_check) { # turn to function
    if (sync_frequency == "hourly") {
      return(now >= (then + lubridate::dhours(1)))
    }
    if (sync_frequency == "daily") {
      return(now >= (then + lubridate::ddays(1)))
    }
    if (sync_frequency == "weekly") {
      return(now >= (then + lubridate::dweeks(1)))
    }
    if (sync_frequency == "monthly") {
      return(now >= (then + lubridate::dmonths(1)))
    }
  }
  TRUE
}
# for if others are using the same object
sweep_dirs_for_cache <- function(project_names = NULL) {
  projects <- get_projects()
  if (nrow(projects) > 0) {
    project_list <- projects %>% split(projects$short_name)
    had_change <- FALSE
    all_project_names <- names(project_list)
    if (is.null(project_names)) {
      project_names <- all_project_names
    }
    project_names <- project_names[which(project_names %in% all_project_names)]
    for (project_name in project_names) {
      from_cache <- project_list[[project_name]]
      expected_path <- get_project_path(short_name = project_name,
                                        dir_path = from_cache$dir_path,
                                        type = "details")
      if (file.exists(expected_path)) {
        to_cache <- tryCatch(
          expr = {
            suppressWarnings({
              readRDS(expected_path)
            })
          },
          error = function(e) {
            NULL
          }
        )
        if (is.null(to_cache)) {
          cli_alert_warning(
            paste0("issue loading project_details: ", project_name)
          )
          to_cache <- from_cache
          had_change <- TRUE
          unlink(expected_path)
        }
        if (!had_change) {
          if (!is.na(from_cache$last_directory_save)) { # should I compare?
            if (to_cache$last_directory_save != from_cache$last_directory_save) {
              if (!identical(to_cache$dir_path, from_cache$dir_path)) {
                to_cache$dir_path <- from_cache$dir_path
                # SAVE
              }
              project_list[[project_name]] <- to_cache
              cli_alert_info(paste0("Updated cache for ", project_name))
              had_change <- TRUE
            }
          }
          # assert_project_details(project_details) #not this
        }
      }
      # else {
      #   # project_list[[project_name]] <- NULL
      #   # cli_alert_info(paste0("Dropped cache for ",project_name," because it didn't exist"))
      #   # had_change <- TRUE
      # }
    }
    if (had_change) {
      projects <- project_list %>% dplyr::bind_rows()
      save_projects_to_cache(projects, silent = FALSE)
    }
  }
}
