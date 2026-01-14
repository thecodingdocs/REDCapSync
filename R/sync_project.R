#' @noRd
sync_project <- function(project,
                         summarize = TRUE,
                         save_to_dir = TRUE,
                         hard_check = FALSE,
                         hard_reset = FALSE) {
  assert_setup_project(project)
  assert_logical(hard_reset,
                 any.missing = FALSE,
                 len = 1L)
  assert_logical(save_to_dir,
                 any.missing = FALSE,
                 len = 1L)
  do_sync <- due_for_sync(project_name = project$project_name) ||
    hard_reset || hard_check
  was_updated <- FALSE
  if (!do_sync) {
    cli_alert_info(
      paste0("{project$project_name} not due for sync ",
             "({project$internals$sync_frequency})"))
  }
  if (do_sync) {
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
    # if (is_something(project$transformation$data_updates)) {
    #   cli_alert_wrap(paste0(
    #   "There is data in 'project$transformation$data_updates' that has not ",
    #     "been uploaded to REDCap yet..."))
    # }
    if (!hard_reset) {
      # check log interim
      if (!is_something(project$internals$last_metadata_update) ||
          !is_something(project$internals$last_data_update) ||
          !is_something(project$internals$last_full_update)) {
        hard_reset <- TRUE
      } else {
        if (!project$redcap$has_log_access) {
          cli_alert_danger(
            paste0( # fix for when access is changed
              "You do not have logging access to this REDCap project. If you ",
              "want to refresh the data use `project$sync(hard_reset = TRUE)`.",
              " Request logging access for more effecient use of API exports."
            )
          )
          return(project)
        }
        interim_log <- get_redcap_log(project, log_begin_date = as.Date(
          strptime(project$redcap$log$timestamp[1L], format = "%Y-%m-%d")
        )) |> unique()
        if (nrow(interim_log) <= nrow(project$redcap$log)) {
          head_of_log <- project$redcap$log |>
            utils::head(n = nrow(interim_log))
        } else {
          head_of_log <- project$redcap$log
        }
        unique_log <- interim_log |>
          rbind(head_of_log) |>
          unique()
        # dup <- unique_log[
        #which(duplicated(
        #rbind(unique_log, head_of_log),fromLast = TRUE)[
        #seq_len(nrow(unique_log)]), ]
        interim_log <- unique_log[
          which(!duplicated(
            rbind(unique_log, head_of_log), fromLast = TRUE)[
              seq_len(nrow(unique_log))]), ]
        if (nrow(interim_log) > 0L) {
          project$redcap$log <- interim_log |>
            bind_rows(project$redcap$log) |>
            unique()
          interim_log$timestamp <- NULL
          interim_log_metadata <- interim_log[
            which(is.na(interim_log$record)), ]
          # inclusion
          interim_log_metadata <- interim_log_metadata[
            which(
              interim_log_metadata$action_type == "Metadata Change Major"), ]
          interim_log_metadata_minor <-
            any(interim_log_metadata$action_type == "Metadata Change Minor")
          # interim_log_metadata <-
          #interim_log_metadata[
          # grep(ignore_redcap_log(),
          #interim_log_metadata$details,ignore.case = TRUE,invert = TRUE) |>
          #unique(),]
          if (nrow(interim_log_metadata) > 0L ||
              interim_log_metadata_minor) {
            # account for minor changes later
            hard_reset <- TRUE
            message("Update because: Metadata was changed!")
          } else {
            interim_log_data <-
              interim_log[which(!is.na(interim_log$record)), ]
            interim_log_data <-
              interim_log_data[which(interim_log_data$action_type != "Users"), ]
            deleted_records <-
              interim_log_data$record[
                which(interim_log_data$action_type == "Delete")]
            if (length(deleted_records) > 0L) {
              warning(
                "There were recent records deleted from redcap",
                "Consider running with 'hard_reset = TRUE'. Records: ",
                toString(deleted_records),
                immediate. = TRUE
              )
            }
            stale_records <- unique(interim_log_data$record)
            if (length(stale_records) == 0L) {
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
      project <- get_redcap_metadata(
        project = project,
        include_users = !project$internals$metadata_only)
      project <- update_project_links(project)
      if (!project$internals$metadata_only) {
        project$data <- list()
        project$data_updates <- list()
        project$summary <- list()
        project$data <- get_redcap_data(
          project = project,
          labelled = project$internals$labelled,
          batch_size = project$internals$batch_size_download
        )
        # if error records comma
        redcap_log <- project$redcap$log # in case there is a log already
        if (project$redcap$has_log_access) {
          if (project$internals$entire_log) {
            log_begin_date <-
              as.POSIXct(project$redcap$project_info$creation_time) |>
              as.Date()
          } else {
            log_begin_date <- Sys.Date() - project$internals$days_of_log
          }
          new_log <- get_redcap_log(project, log_begin_date = log_begin_date)
          project$redcap$log <- redcap_log |>
            bind_rows(new_log) |>
            sort_redcap_log()
        }
        project$summary$all_records <- extract_project_records(project)
        project$summary$all_records$last_api_call <-
          project$internals$last_full_update <-
          project$internals$last_metadata_update <-
          project$internals$last_data_update <- now_time()
        cli_alert_wrap(paste0("Full ", project$project_name, " update!"),
                       bullet_type = "v")
        was_updated <- TRUE
      }
    } else {
      if (will_update) {
        id_col <- project$metadata$id_col
        project$data <- all_character_cols_list(project$data)
        if (length(deleted_records) > 0L) {
          stale_records <-
            stale_records[which(!stale_records %in% deleted_records)]
          project <-
            remove_records_from_project(
              project = project, records = deleted_records)
          x <- !project$summary$all_records[[id_col]] %in% deleted_records
          project$summary$all_records <- project$summary$all_records[which(x), ]
        }
        message_string <- "No new records to update!"
        if (length(stale_records) > 0L) {
          form_list <- project |>
            get_redcap_data(
              labelled = project$internals$labelled,
              records = stale_records) |>
            all_character_cols_list()
          missing_from_summary <- stale_records[
            which(!stale_records %in% project$summary$all_records[[id_col]])]
          if (length(missing_from_summary) > 0L) {
            x <- data.frame(
              record = missing_from_summary,
              last_api_call = NA,
              was_saved = FALSE,
              stringsAsFactors = FALSE
            )
            colnames(x)[1L] <- id_col
            project$summary$all_records <-
              bind_rows(project$summary$all_records, x)
            x <- order(project$summary$all_records[[id_col]], decreasing = TRUE)
            project$summary$all_records <- project$summary$all_records[x, ]
          }
          project <- remove_records_from_project(
            project = project, records = stale_records)
          if (!all(names(form_list) %in% project$metadata$forms$form_name)) {
            stop(
              "Imported data names doesn't match project$data names. If this",
              "happens run `project$sync(hard_reset = TRUE)`"
            )
          }
          for (form_name in names(form_list)) {
            project$data[[form_name]] <- project$data[[form_name]] |>
              all_character_cols() |>
              bind_rows(form_list[[form_name]])
          }
          row_match <-
            which(project$summary$all_records[[id_col]] %in% stale_records)
          project$summary$all_records$last_api_call[row_match] <-
            project$internals$last_data_update <-
            now_time()
          project$summary$all_records$was_saved[row_match] <- FALSE
          message_string <- toString(stale_records)
          stale_record_length <- length(stale_records)
          if (stale_record_length > 20L) {
            message_string <- paste(stale_record_length, "records")
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
  if (project$internals$add_default_summaries) {
    if (!is_something(project$summary$REDCapSync) ||
        !is_something(project$summary$REDCapSync_raw)) {
      project <- add_default_summaries(project)
    }
  }
  # if (project$internals$add_default_fields) {
  #   project <- add_default_fields(project)
  # }
  if (save_to_dir && !is.null(project$dir_path)) {
    if (is_something(project$data)) {
      if (project$internals$get_files) {
        get_redcap_files(
          # would want track internally?
          project,
          original_file_names = project$internals$original_file_names
        )
      }
    }
    if (project$internals$get_file_repository) {
      # get_redcap_file_repository()
    }
    if (summarize) {
      first_stamp <- project$internals$last_summary
      project <- summarize_project(project = project, hard_reset = hard_reset)
      second_stamp <- project$internals$last_summary
      was_updated <- was_updated ||
        !identical(first_stamp, second_stamp)
    }
    if (was_updated) {
      project <- save_project(project)
    } else {
      project$internals$last_directory_save <- project$internals$last_sync
      project |> extract_project_details() |> add_project_details_to_cache()
    }
  }
  invisible(project)
}
#' @title Synchronize REDCap Data
#' @description
#' Syncs with REDCap via `project` object that user defined with
#' \link{setup_project}
#'
#' @details
#' Syncs all projects by default but can be used to hands-free sync one or
#' defined set projects. This is not intended to return project object. User
#' should use `load_project("project_name")`. However, by default will invisibly
#' return the last project in the set of project_names.
#'
#' @param summarize Logical (TRUE/FALSE). If TRUE, summarizes data to directory.
#' @param hard_check Will check REDCap even if not due (see `sync_frequency`
#' parameter from `setup_project()`)
#' @param hard_reset Logical that forces a fresh update if TRUE. Default is
#' `FALSE`.
#' @param project_names character vector of project project_names previously
#' setup. If NULL, will get all from `get_projects()`
#' @return invisible return of last project
#' @seealso
#' \link{setup_project} for initializing the `project` object.
#' @export
sync <- function(project_names = NULL,
                 summarize = TRUE,
                 hard_check = FALSE,
                 hard_reset = FALSE) {
  sweep_dirs_for_cache(project_names = project_names)
  if (is.null(project_names)) {
    projects <- get_projects()
    project_names <- projects$project_name
    if (length(project_names) == 0L) {
      cli_alert_info("No projects in cache. Start with `?setup_project()`")
      return(invisible())
    }
  }
  for (project_name in project_names) {
    project <- tryCatch(
      expr = load_project(project_name),
      error = function(e) {
        NULL
      })
    if (is.null(project)) {
      cli_alert_danger("Unable to load {project_name}")
      #add to bad list
    } else {
      project$sync(
        save_to_dir = TRUE,
        summarize = summarize,
        hard_check = hard_check,
        hard_reset = hard_reset
      )
    }
  }
  # consider adding message/df
  cli_alert_success("All projects are synced!")
  invisible(project)
}
#' @noRd
due_for_sync <- function(project_name) {
  now <- now_time()
  projects <- get_projects()
  if (project_name %in% projects$project_name) {
    assert_names(projects$project_name, must.include = project_name)
    project_row <- which(projects$project_name == project_name)
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
    have_to_check <-
      sync_frequency %in% c("hourly", "daily", "weekly", "monthly")
    if (have_to_check) {
      # turn to function
      if (sync_frequency == "hourly") {
        return(now >= (then + lubridate::dhours(1L)))
      }
      if (sync_frequency == "daily") {
        return(now >= (then + lubridate::ddays(1L)))
      }
      if (sync_frequency == "weekly") {
        return(now >= (then + lubridate::dweeks(1L)))
      }
      if (sync_frequency == "monthly") {
        return(now >= (then + lubridate::dmonths(1L)))
      }
    }
  }
  TRUE
}
# for if others are using the same object
#' @noRd
sweep_dirs_for_cache <- function(project_names = NULL) {
  projects <- get_projects()
  if (nrow(projects) == 0L) {
    return(invisible())
  }
  project_list <- split(projects, projects$project_name)
  had_change <- FALSE
  all_project_names <- names(project_list)
  if (is.null(project_names)) {
    project_names <- all_project_names
  }
  project_names <- project_names[which(project_names %in% all_project_names)]
  updated_projects <- NULL
  for (project_name in project_names) {
    from_cache <- project_list[[project_name]]
    expected_path <- get_project_path(
      project_name = project_name,
      dir_path = from_cache$dir_path,
      type = "details"
    )
    from_cache <- tryCatch(
      expr = assert_project_details(from_cache, nrows = 1L),
      error = function(e) {
        NULL
      })
    to_cache <- NULL
    if (file.exists(expected_path)) {
      to_cache <- tryCatch(
        expr = {
          x <- suppressWarnings({
            readRDS(expected_path)
          })
          assert_project_details(x, nrows = 1L)
          x
        },
        error = function(e) {
          NULL
        }
      )
    }
    if (is.null(from_cache) || is.null(to_cache)) {
      loaded_cache <- tryCatch(
        expr = {
          load_project(project_name = project_name)$.internal() |>
            extract_project_details()
        },
        error = function(e) {
          NULL
        }
      )
      if (is.null(loaded_cache)) {
        cli_alert_danger(
          paste0("Unable to load ", project_name, ". Removed! Retry ",
                 "`setup_project(...)`"))
        project_list[[project_name]] <- NULL
        had_change <- TRUE
      } else {
        project_list[[project_name]] <- loaded_cache
      }
    }
    if (!is.null(to_cache)) {
      if (!identical(from_cache, to_cache)) {
        if (!is.null(from_cache)) {
          to_cache$dir_path <- from_cache$dir_path
        } # could cause issue?
        project_list[[project_name]] <- to_cache
        updated_projects <-  append(updated_projects, project_name)
        had_change <- TRUE
      }
    }
  }
  if (had_change) {
    cli_alert_info(
      paste0("Updated cache! This can happen when using multiple",
             " computers or with version changes..."))
    save_projects_to_cache(bind_rows(project_list), silent = FALSE)
  }
}
