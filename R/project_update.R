#' @title Update REDCap Database
#' @description
#' Updates the REDCap database (`project` object) by fetching the latest data from
#' the REDCap server.
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
#' @param force Logical that forces a fresh update if TRUE. Default is `FALSE`.
#' @param day_of_log Integer. Number of days to be checked in the log. Default
#' is `10`.
#' @param labelled Logical (TRUE/FALSE). If TRUE, returns labelled REDCap data.
#' If FALSE, returns raw data. Default is `TRUE`.
#' @param get_files Logical (TRUE/FALSE). If TRUE, retrieves files from REDCap.
#' Default is `FALSE`.
#' @param original_file_names Logical (TRUE/FALSE). If TRUE, uses original file
#' names for retrieved files. Default is `FALSE`.
#' @param entire_log Logical (TRUE/FALSE). If TRUE, retrieves the entire log.
#' Default is `FALSE`.
#' @param metadata_only Logical (TRUE/FALSE). If TRUE, updates only the
#' metadata. Default is `FALSE`.
#' @param ask_about_overwrites Logical (TRUE/FALSE). If TRUE, prompts the user
#' before overwriting existing data. Default is `TRUE`.
#' @param save_to_dir Logical (TRUE/FALSE). If TRUE, saves the updated data to
#' the directory. Default is `TRUE`.
#' @param records optional records character vector
#' @param batch_size Integer. Number of records to process in each batch.
#' Default is `2000`.
#' @return Messages for confirmation.
#' @seealso
#' \link{setup_project} for initializing the `project` object.
#' @family db_functions
#' @export
sync_project <- function(
    project,
    set_token_if_fails = TRUE,
    force = FALSE,
    day_of_log = 10,
    labelled = TRUE,
    get_files = FALSE,
    original_file_names = FALSE,
    entire_log = FALSE,
    metadata_only = FALSE,
    ask_about_overwrites = TRUE,
    save_to_dir = TRUE,
    records = NULL,
    batch_size = 2000) {
  IDs <- NULL
  will_update <- TRUE
  was_updated <- FALSE
  project <- validate_project(project)
  if (is_something(records)) {
    bullet_in_console("Presently, if you supply specified records it will only check REDCap updates for those records.")
  }
  if (!is.null(project$internals$data_extract_labelled)) {
    if (project$internals$data_extract_labelled != labelled) {
      if (!force) {
        load_type <- ifelse(project$internals$data_extract_labelled, "labelled", "raw")
        chosen_type <- ifelse(labelled, "labelled", "raw")
        force <- TRUE
        warning(
          "The project that was loaded was ",
          load_type, " and you chose ", chosen_type,
          ". Therefore, a full update was triggered to avoid data conflicts",
          immediate. = TRUE
        )
      }
    }
  }
  project <- test_REDCap_token(project, set_if_fails = set_token_if_fails)
  connected <- project$internals$last_test_connection_outcome
  if (!connected) {
    bullet_in_console("Could not connect to REDCap", bullet_type = "x")
    return(project)
  }
  if (metadata_only) force <- TRUE
  # project$internals$last_metadata_update <- Sys.time()-lubridate::days(1)
  # project$internals$last_data_update <- Sys.time()-lubridate::days(1)
  if (!is.null(project$transformation$data_updates)) {
    do_it <- TRUE
    bullet_in_console("There is data in 'project$transformation$data_updates' that has not been pushed to REDCap yet...")
    print(project$transformation$data_updates)
    if (ask_about_overwrites) {
      do_it <- utils::menu(choices = c("Yes", "No and stop the function!"), title = "Would you like to push these updates now?") == 1
    }
    if (!do_it) stop("Stopped as requested!")
    project <- upload_transform_to_project(project)
  }
  if (!force) { # check log interim
    if (
      is.null(project$internals$last_metadata_update) ||
      is.null(project$internals$last_data_update) ||
      is.null(project$internals$last_full_update)
    ) {
      force <- TRUE
    } else {
      ilog <- get_REDCap_log(
        project,
        begin_time = as.character(strptime(project$redcap$log$timestamp[1], format = "%Y-%m-%d %H:%M") - lubridate::days(1))
      ) %>% clean_redcap_log()
      if (nrow(ilog) <= nrow(project$redcap$log)) {
        head_of_log <- project$redcap$log %>% utils::head(n = nrow(ilog))
      } else {
        head_of_log <- project$redcap$log
      }
      df1 <- ilog %>%
        rbind(head_of_log) %>%
        unique()
      # dup <- df1[which(duplicated(rbind(df1, head_of_log),fromLast = TRUE)[seq_len(nrow(df1)]), ]
      ilog <- df1[which(!duplicated(rbind(df1, head_of_log), fromLast = TRUE)[seq_len(nrow(df1))]), ]
      if (nrow(ilog) > 0) {
        project$redcap$log <- ilog %>%
          dplyr::bind_rows(project$redcap$log) %>%
          unique()
        ilog$timestamp <- NULL
        ilog_metadata <- ilog[which(is.na(ilog$record)), ]
        ilog_metadata <- ilog_metadata[which(ilog_metadata$action_type %in% "Metadata Change Major"), ] # inclusion
        ilog_metadata_minor <- length(which(ilog_metadata$action_type %in% "Metadata Change Minor")) > 0
        # ilog_metadata <- ilog_metadata[grep(ignore_redcap_log(),ilog_metadata$details,ignore.case = TRUE,invert = TRUE) %>% unique(),]
        if (nrow(ilog_metadata) > 0 || ilog_metadata_minor) { # account for minor changes later
          force <- TRUE
          message(paste0("Update because: Metadata was changed!"))
        } else {
          ilog_data <- ilog[which(!is.na(ilog$record)), ]
          ilog_data <- ilog_data[which(ilog_data$action_type != "Users"), ]
          if (is_something(records)) {
            ilog_data <- ilog_data[which(ilog$record %in% records), ]
          }
          deleted_records <- ilog_data$record[which(ilog_data$action_type %in% c("Delete"))]
          if (length(deleted_records) > 0) {
            warning("There were recent records deleted from redcap Consider running with 'force = TRUE'. Records: ", deleted_records %>% paste0(collapse = ", "), immediate. = TRUE)
          }
          IDs <- ilog_data$record %>% unique()
          if (length(IDs) == 0) {
            IDs <- NULL
            will_update <- FALSE
          }
        }
      } else {
        will_update <- FALSE
      }
    }
  }
  if (force) {
    project <- project %>% get_REDCap_metadata(include_users = !metadata_only)
    project$internals$is_transformed <- FALSE
    if (!metadata_only) {
      project$data <- list()
      project$data_update <- list()
      project$summary <- list()
      project$data <- project %>% get_REDCap_data(labelled = labelled, batch_size = batch_size, records = records)
      project$internals$data_extract_labelled <- labelled
      log <- project$redcap$log # in case there is a log already
      if (entire_log) {
        project$redcap$log <- log %>% dplyr::bind_rows(
          project %>% get_REDCap_log(begin_time = project$redcap$project_info$creation_time) %>% unique() # should add - lubridate::days(2)
        )
      } else {
        project$redcap$log <- log %>% dplyr::bind_rows(
          project %>% get_REDCap_log(last = day_of_log, units = "days") %>% unique()
        )
      }
      # project <- annotate_fields(project)
      # project <- annotate_choices(project)
      project$summary$all_records <- sum_records(project)
      project$summary$all_records$last_api_call <-
        project$internals$last_full_update <-
        project$internals$last_metadata_update <-
        project$internals$last_data_update <- Sys.time()
      bullet_in_console(paste0("Full ", project$short_name, " update!"), bullet_type = "v")
      was_updated <- TRUE
    }
  } else {
    # if(ilog_metadata_minor){
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
        IDs <- IDs[which(!IDs %in% deleted_records)]
        project$data <- remove_records_from_list(project = project, records = deleted_records, silent = TRUE)
      }
      data_list <- project %>% get_REDCap_data(labelled = labelled, records = IDs)
      missing_from_summary <- IDs[which(!IDs %in% project$summary$all_records[[project$redcap$id_col]])]
      if (length(missing_from_summary) > 0) {
        x <- data.frame(
          record = missing_from_summary,
          last_api_call = NA
        )
        colnames(x)[1] <- project$redcap$id_col
        project$summary$all_records <- project$summary$all_records %>% dplyr::bind_rows(x)
        project$summary$all_records <- project$summary$all_records[order(project$summary$all_records[[project$redcap$id_col]], decreasing = TRUE), ]
      }
      project$summary$all_records$last_api_call[which(project$summary$all_records[[project$redcap$id_col]] %in% IDs)] <-
        project$internals$last_data_update <-
        Sys.time()
      project$data <- remove_records_from_list(project = project, records = IDs, silent = TRUE)
      if (project$internals$is_transformed) {
        project2 <- stripped_project(project)
        project2$internals$is_transformed <- FALSE
        project2$metadata$forms <- project2$transformation$original_forms
        project2$metadata$fields <- project2$transformation$original_fields
        project2$data <- data_list
        project2 <- transform_project(project2, ask = ask_about_overwrites)
        if (!is.null(project2$data_update$from_transform)) {
          project2 <- upload_transform_to_project(project2)
        }
        data_list <- project2$data %>%
          process_df_list(silent = TRUE) %>%
          all_character_cols_list()
      }
      if (any(!names(data_list) %in% names(project$data))) stop("Imported data names doesn't match project$data names. If this happens run `untransform_project()` or `sync_project(project, force = TRUE)`")
      for (TABLE in names(data_list)) {
        project$data[[TABLE]] <- project$data[[TABLE]] %>%
          all_character_cols() %>%
          dplyr::bind_rows(data_list[[TABLE]])
      }
      message("Updated: ", paste0(IDs, collapse = ", "))
      was_updated <- TRUE
    } else {
      message("Up to date already!")
    }
  }
  if (get_files) { # test now
    get_REDCap_files(project, original_file_names = original_file_names)
  }
  if (was_updated && save_to_dir && !is.null(project$dir_path)) {
    save_project(project)
  }
  project
}
