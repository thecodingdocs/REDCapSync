#' @title Syncronize REDCap Data
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
#' @param set_token_if_fails Logical (TRUE/FALSE). If TRUE, prompts the user to
#' set the REDCap API token if the update fails. Default is `TRUE`.
#' @param reset Logical that forces a fresh update if TRUE. Default is `FALSE`.
#' @param ask_about_overwrites Logical (TRUE/FALSE). If TRUE, prompts the user
#' #' before overwriting existing data. Default is `TRUE`.
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
    set_token_if_fails = TRUE,
    reset = FALSE,
    silent = FALSE,
    ask_about_overwrites = TRUE,
    summarize = TRUE,
    save_to_dir = TRUE) {
  collected <- makeAssertCollection()
  assert_blank_project(project)
  assert_logical(
    set_token_if_fails,
    any.missing = FALSE,
    len = 1,
    add = collected
  )
  assert_logical(reset, any.missing = FALSE, len = 1, add = collected)
  assert_logical(
    ask_about_overwrites,
    any.missing = FALSE,
    len = 1,
    add = collected
  )
  assert_logical(save_to_dir, any.missing = FALSE, len = 1, add = collected)
  assert_logical(silent, any.missing = FALSE, len = 1, add = collected)
  current_function <- as.character(current_call())[[1]]
  if (!collected$isEmpty()) {
    message <- collected %>% cli_message_maker(function_name = current_function)
    cli::cli_abort(message)
  }
  do_it <- due_for_sync(project_name = project$short_name) || reset
  if (!do_it) {
    cli::cli_alert_info("{project$short_name} not due for sync ({project$internals$sync_frequency})")
    return(invisible(project))
  }
  stale_records <- NULL
  will_update <- TRUE
  was_updated <- FALSE
  # project$internals$last_directory_save
  # project$internals$last_test_connection_attempt
  project <- test_project_token(project, set_if_fails = set_token_if_fails)
  connected <- project$internals$last_test_connection_outcome
  if (!connected) {
    bullet_in_console(
      "Could not connect to REDCap",
      bullet_type = "x",
      silent = silent
    )
    return(invisible(project))
  }
  # project$internals$last_metadata_update <-  now_time()-lubridate::days(1)
  # project$internals$last_data_update <-  now_time()-lubridate::days(1)
  if (is_something(project$transformation$data_updates)) {
    do_it <- TRUE
    bullet_in_console(
      "There is data in 'project$transformation$data_updates' that has not been pushed to REDCap yet..."
    )
    print(project$transformation$data_updates)
    if (ask_about_overwrites) {
      do_it <- utils::menu(
        choices = c("Yes", "No and stop the function!"),
        title = "Would you like to push these updates now?"
      ) == 1
    }
    if (!do_it) stop("Stopped as requested!")
    project <- upload_transform_to_project(project)
  }
  if (!reset) { # check log interim
    if (
      !is_something(project$internals$last_metadata_update) ||
      !is_something(project$internals$last_data_update) ||
      !is_something(project$internals$last_full_update)
    ) {
      reset <- TRUE
    } else {
      interim_log <- get_REDCap_log(
        project,
        log_begin_date = as.Date(strptime(project$redcap$log$timestamp[1], format = "%Y-%m-%d"))
      )
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
        interim_log_metadata <- interim_log_metadata[which(interim_log_metadata$action_type %in% "Metadata Change Major"), ] # inclusion
        interim_log_metadata_minor <- length(which(interim_log_metadata$action_type %in% "Metadata Change Minor")) > 0
        # interim_log_metadata <- interim_log_metadata[grep(ignore_redcap_log(),interim_log_metadata$details,ignore.case = TRUE,invert = TRUE) %>% unique(),]
        if (nrow(interim_log_metadata) > 0 || interim_log_metadata_minor) { # account for minor changes later
          reset <- TRUE
          message(paste0("Update because: Metadata was changed!"))
        } else {
          interim_log_data <- interim_log[which(!is.na(interim_log$record)), ]
          interim_log_data <- interim_log_data[which(interim_log_data$action_type != "Users"), ]
          deleted_records <- interim_log_data$record[which(interim_log_data$action_type %in% c("Delete"))]
          if (length(deleted_records) > 0) {
            warning("There were recent records deleted from redcap Consider running with 'reset = TRUE'. Records: ", deleted_records %>% paste0(collapse = ", "), immediate. = TRUE)
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
    project$internals$is_transformed <- FALSE
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
        # if(is_something(log)){
        #   log_begin_date <- (log$timestamp[1] %>% as.Date())-1
        #   #account for renames in log would have to get entire log again, should separate reset from going to get one new variable etc
        # } # need log check for what is missing
        project$redcap$log <- log %>%
          dplyr::bind_rows(
            project %>% get_REDCap_log(
              log_begin_date = log_begin_date
            ) %>%
              unique() # should add - lubridate::days(2)
          ) %>%
          sort_redcap_log()
      } else {
        project$redcap$log <- log %>%
          dplyr::bind_rows(
            project %>% get_REDCap_log(
              log_begin_date = Sys.Date() - project$internals$days_of_log
            ) %>%
              unique()
          ) %>%
          sort_redcap_log()
      }
      # project <- annotate_fields(project)
      # project <- annotate_choices(project)
      project$summary$all_records <- sum_records(project)
      project$summary$all_records$last_api_call <-
        project$internals$last_full_update <-
        project$internals$last_metadata_update <-
        project$internals$last_data_update <- now_time()
      bullet_in_console(paste0("Full ", project$short_name, " update!"), bullet_type = "v")
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
        project$summary$all_records <- project$summary$all_records[which(!project$summary$all_records[[project$redcap$id_col]] %in% deleted_records), ]
        stale_records <- stale_records[which(!stale_records %in% deleted_records)]
        project$data <- remove_records_from_list(project = project, records = deleted_records, silent = TRUE)
      }
      form_list <- project %>% get_REDCap_data(labelled = project$internals$labelled, records = stale_records)
      missing_from_summary <- stale_records[which(!stale_records %in% project$summary$all_records[[project$redcap$id_col]])]
      if (length(missing_from_summary) > 0) {
        x <- data.frame(
          record = missing_from_summary,
          last_api_call = NA
        )
        colnames(x)[1] <- project$redcap$id_col
        project$summary$all_records <- project$summary$all_records %>% dplyr::bind_rows(x)
        project$summary$all_records <- project$summary$all_records[order(project$summary$all_records[[project$redcap$id_col]], decreasing = TRUE), ]
      }
      project$summary$all_records$last_api_call[which(project$summary$all_records[[project$redcap$id_col]] %in% stale_records)] <-
        project$internals$last_data_update <-
        now_time()
      project$data <- remove_records_from_list(project = project, records = stale_records, silent = TRUE)
      # if (project$internals$is_transformed) {
      #   project2 <- stripped_project(project)
      #   project2$internals$is_transformed <- FALSE
      #   project2$data <- form_list
      #   # add remove records from list equivalant
      #   project2 <- transform_project(project2)
      #   if (!is.null(project2$data_updates$from_transform)) {
      #     do_it <- TRUE
      #     bullet_in_console("There is data in 'project$transformation$data_updates' that has not been pushed to REDCap yet...")
      #     print(project2$transformation$data_updates)
      #     if (ask_about_overwrites) {
      #       do_it <- utils::menu(choices = c("Yes", "No and stop the function!"), title = "Would you like to push these updates now?") == 1
      #     }
      #     if (!do_it) stop("Stopped as requested!")
      #     # account for uploads in process
      #     project2 <- upload_transform_to_project(project2)
      #   }
      #   form_list <- project2$data %>%
      #     process_df_list(silent = TRUE) %>%
      #     all_character_cols_list()
      # }
      if (any(!names(form_list) %in% names(project$data))) stop("Imported data names doesn't match project$data names. If this happens run `sync_project(project, reset = TRUE)`")
      for (form_name in names(form_list)) {
        project$data[[form_name]] <- project$data[[form_name]] %>%
          all_character_cols() %>%
          dplyr::bind_rows(form_list[[form_name]])
      }
      message("Updated: ", paste0(stale_records, collapse = ", "))
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
  if (save_to_dir && !is.null(project$dir_path)) {
    project <- drop_REDCap_to_directory(
      project = project,
      smart = TRUE,
      deidentify = FALSE,
      include_metadata = TRUE,
      include_other = TRUE,
      separate = TRUE
    ) # add params
    # vars
    # transform
    # if(transform) {
    #   project <- transform_project(
    #   )
    # }
    if (summarize) {
      project <- summarize_project(
        project = project,
        reset = reset
      )
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
