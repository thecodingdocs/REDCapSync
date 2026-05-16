#' @title Synchronize REDCap Data
#' @description
#' Syncs with REDCap via `project` object that user defined with
#' \link{setup_project}
#' @details
#' Syncs all projects by default but can be used to hands-free sync one or
#' defined set projects. This is not intended to return project object. User
#' should use `load_project("project_name")`. However, by default will invisibly
#' return the last project in the set of project_names.
#' @param save_datasets Logical (TRUE/FALSE). If TRUE, saves datasets to
#' directory.
#' @param hard_check Will check REDCap even if not due (see `sync_frequency`
#' parameter from `setup_project()`)
#' @param hard_reset Logical that forces a fresh update if TRUE. Default is
#' `FALSE`.
#' @param project_names character vector of project project_names previously
#' setup. If NULL, will get all from `get_projects()`
#' @examples
#' \dontrun{
#' # if you setup 3 projects "ONE", "TWO", "THREE" using [setup_project()]...
#' sync() # will check all three projects for updates if due for sync
#' sync("TWO") # will only check and sync project TWO
#' project <- sync("TWO") # can also be used for invisible return ...
#' sync(c("ONE","THREE")) # will only check and sync projects ONE and THREE
#' project <- sync(c("ONE","THREE")) # invisible return of THREE ...
#' }
#' @returns Invisibly returns last project object (R6 [REDCapSyncProject] class)
#' @seealso
#' \link{setup_project} for initializing the `project` object.
#' @export
sync <- function(project_names = NULL,
                 save_datasets = TRUE,
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
  cli_progress_bar("Syncing REDCap Projects", total = length(project_names))
  for (project_name in project_names) {
    project <- try_else_null(load_project(project_name))
    if (is.null(project)) {
      cli_alert_danger("Unable to load {project_name}")
      #add to bad list
    } else {
      project$sync(
        save_to_dir = TRUE,
        save_datasets = save_datasets,
        hard_check = hard_check,
        hard_reset = hard_reset
      )
    }
    cli_progress_update()
  }
  # consider adding message/df
  cli_alert_success("All projects are synced!")
  invisible(project)
}
#' @noRd
sync_project <- function(project,
                         save_datasets = TRUE,
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
  hard_reset <- hard_reset || project$internals$hard_reset
  do_sync <- due_for_sync(project_name = project$project_name) ||
    hard_reset || hard_check
  was_updated <- FALSE
  if (!do_sync) {
    info_message <- paste0("{project$project_name} not due for sync ",
                           "({project$settings$sync_frequency})")
    cli_alert_info(info_message)
  }
  if (do_sync) {
    project <- sync_project_check(project = project, hard_reset = hard_reset)
    was_updated <- project$internals$was_updated
  }
  if (project$settings$add_default_datasets) {
    missing_redcapsync <- !is_something(project$datasets$REDCapSync)
    missing_redcapsync_raw <- !is_something(project$datasets$REDCapSync_raw)
    if (missing_redcapsync || missing_redcapsync_raw) {
      project <- add_default_datasets(project)
    }
  }
  # ?add_default_fields
  if (save_to_dir && !is.null(project$dir_path)) {
    if (is_something(project$data) && project$settings$get_files) {
      get_redcap_files(
        # would want track internally?
        project,
        original_file_names = project$settings$original_file_names,
        overwrite = TRUE
      ) # account for needed overwrites from new records
    }
    if (project$settings$get_file_repository) {
      # get redcap file repo
    }
    if (save_datasets) {
      first_stamp <- project$internals$last_dataset_save
      project <- save_project_datasets(project = project,
                                       hard_reset = hard_reset)
      second_stamp <- project$internals$last_dataset_save
      was_updated <- was_updated ||
        !identical(first_stamp, second_stamp)
    }
    if (was_updated) {
      project <- save_project(project)
    } else {
      project$internals$last_directory_save <- project$internals$last_sync
      project_details <- extract_project_details(project)
      saveRDS(
        object = project_details,
        file = get_project_path2(project, type = "details")
      ) # add error check
      add_project_details_to_cache(project_details)
    }
  }
  invisible(project)
}
#' @noRd
sync_project_hard_reset <- function(project) {
  assert_setup_project(project)
  project <- get_redcap_metadata(project = project)
  project <- update_project_links(project)
  if (project$settings$get_data) {
    project$data <- list()
    project$datasets <- list()
    project$data <- get_redcap_data(
      project = project,
      labelled = project$settings$labelled
    )
    # if error records comma
    if (project$redcap$has_log_access) {
      if (project$settings$get_entire_log) {
        creation_time <- project$redcap$project_info$creation_time
        log_begin_date <- as.Date(as.POSIXct(creation_time))
        log_begin_date <- log_begin_date - 1L
      } else {
        log_begin_date <- Sys.Date() - project$settings$log_days
      }
      # don't need old log in this case
      project$redcap$log <- get_redcap_log(project = project,
                                           log_begin_date = log_begin_date)
    }
    right_now <- now_time()
    project$internals$last_full_update <-
      project$internals$last_metadata_update <-
      project$internals$last_data_update <- right_now
    record_summary <- extract_project_records(project)
    if (is_something(record_summary)) {
      project$record_summary <- record_summary
      project$record_summary$last_api_call <- right_now
    }
  }
  project$internals$hard_reset <- FALSE
  cli_alert_success("Full {project$project_name} update!")
  project
}
#' @noRd
sync_project_check <- function(project, hard_reset = FALSE, records = NULL) {
  project$internals$was_updated <- FALSE
  project <- test_project_token(project)
  connected <- project$internals$last_test_connection_outcome
  if (!connected) {
    cli_alert_danger("Could not connect to REDCap")
    return(invisible(project))
  }
  hard_reset <- hard_reset ||
    !is_something(project$internals$last_metadata_update) ||
    !is_something(project$internals$last_data_update) ||
    !is_something(project$internals$last_full_update)
  if (!hard_reset) {
    interim_log <- get_interim_log(project)
    if (is_something(interim_log)) {
      log_changes <- analyze_log(interim_log, id_col = project$metadata$id_col)
      log_change_messages(log_changes)
      hard_reset <- log_changes$hard_reset
      project$redcap$log <- interim_log |>
        bind_rows(project$redcap$log) |>
        unique()
      project$internals$was_updated <- TRUE
      if (!hard_reset) {
        if (log_changes$refresh_data || !is.null(records)) {
          deleted_records <- log_changes$deleted_records
          updated_records <- log_changes$updated_records
          refresh_records <- c(deleted_records, updated_records, records)
          refresh_records <- unique(refresh_records)
          project <- sync_project_refresh(project = project,
                                          refresh_records = refresh_records)
          if (is.null(project)) {
            # anything wrong with refresh should force hard_reset
            hard_reset <- TRUE
          }
        }
      }
    }
  }
  if (hard_reset) {
    project <- sync_project_hard_reset(project)
    project$internals$was_updated <- TRUE
  }
  project$internals$last_sync <- now_time()
  project
}
#' @noRd
sync_project_refresh <- function(project, refresh_records) {
  assert_setup_project(project)
  id_col <- project$metadata$id_col
  project$data <- all_character_cols_list(project$data)
  all_records <- get_redcap_records(project)
  project <- remove_records_from_project(project = project,
                                         records = refresh_records)
  form_list <- get_redcap_data(
    project = project,
    labelled = project$settings$labelled,
    records = refresh_records # still checking deletes
  )
  records_received <- extract_values_from_form_list(
    form_list = form_list,
    col_name = id_col
  )
  #check diff with refresh?
  if (!all(names(form_list) %in% project$metadata$forms$form_name)) {
    cli_alert_danger("Full update triggered: data mismatch")
    return(NULL)
  }
  if (!all(records_received %in% all_records)) {
    cli_alert_danger("Full update triggered: data mismatch")
    return(NULL)
  }
  for (form_name in names(form_list)) {
    project$data[[form_name]] <- project$data[[form_name]] |>
      all_character_cols() |>
      bind_rows(form_list[[form_name]])
    reorder_records <- match(all_records, project$data[[form_name]][[id_col]])
    project$data[[form_name]] <- project$data[[form_name]][reorder_records, ]
    rownames(project$data[[form_name]]) <- NULL
  }
  records_dataset <- project$record_summary[[id_col]]
  missing_records_i <- which(!records_received %in% records_dataset)
  missing_from_dataset <- records_received[missing_records_i]
  if (length(missing_from_dataset) > 0L) {
    x <- data.frame(
      record = missing_from_dataset,
      last_api_call = NA,
      was_saved = FALSE,
      stringsAsFactors = FALSE
    )
    colnames(x)[1L] <- id_col
    project$record_summary <- bind_rows(project$record_summary, x)
    new_order <- order(project$record_summary[[id_col]], decreasing = TRUE)
    project$record_summary <- project$record_summary[new_order, ]
  }
  project$internals$last_data_update <- now_time()
  row_new <- which(project$record_summary[[id_col]] %in% records_received)
  project$record_summary$last_api_call[row_new] <-
    project$internals$last_data_update
  project$record_summary$was_saved[row_new] <- FALSE
  project$record_summary
  reorder_records <- match(all_records, project$record_summary[[id_col]])
  project$record_summary <- project$record_summary[reorder_records, ]
  rownames(project$record_summary) <- NULL
  invisible(project)
}
#' @noRd
generate_comment_table <- function(redcap_log, only_most_recent = FALSE) {
  assert_data_frame(redcap_log)
  assert_logical(only_most_recent)
  redcap_log <- redcap_log[which(redcap_log$action_type == "Comment"), ]
  redcap_log$comment_type <- redcap_log$details |>
    strsplit(" ") |>
    lapply(first) |>
    unlist()
  inner <- sub("^.*?\\((.*)\\)$", "\\1", redcap_log$details)
  redcap_log$comment_field <- str_match(inner, ", Field:\\s([^,]+)")[, 2L]
  if (only_most_recent && is_something(redcap_log)) {
    redcap_log <- redcap_log |>
      split(redcap_log$record) |>
      lapply(function(record) {
        record |> split(record$comment_field) |> lapply(first) |>  bind_rows()
      }) |>
      bind_rows()
    keep_rows <- which(redcap_log$comment_type != "Delete")
    redcap_log <- redcap_log[keep_rows, ]
    inner <- sub("^.*?\\((.*)\\)$", "\\1", redcap_log$details)
  }
  if (nrow(redcap_log) == 0L) {
    return(NULL)
  }
  redcap_log$comment_details <- inner |>
    str_remove("^.*, Comment:\\s\"") |>
    str_remove("\"$")
  redcap_log$comment_details[which(
    redcap_log$comment_details == paste0(
      "Record: ",
      redcap_log$record,
      ", Field: ",
      redcap_log$comment_field
    )
  )] <- NA
  redcap_log
}
#' @noRd
due_for_sync <- function(project_name) {
  now <- now_time()
  projects <- get_projects()
  if (project_name %in% projects$project_name) {
    assert_names(projects$project_name, must.include = project_name)
    project_row <- which(projects$project_name == project_name)
    last_sync <- projects$last_sync[project_row]
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
    if (sync_frequency == "once") {
      return(is.na(then))
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
#' @noRd
remove_from_form_list <- function(form_list, id_col, records = NULL) {
  if (!is_something(form_list)) {
    return(form_list)
  }
  if (!is_df_list(form_list)) {
    cli_abort("form_list is not a list of data.frames as expected.")
  }
  if (is.null(records)) {
    return(form_list)
  }
  rows_for_forms <- which(unlist(lapply(names(form_list), function(form_name) {
    nrow(form_list[[form_name]]) > 0L
  })))
  form_names <- names(form_list)[rows_for_forms]
  for (form_name in form_names) {
    chosen_rows <- which(!form_list[[form_name]][[id_col]] %in% records)
    form_list[[form_name]] <- form_list[[form_name]][chosen_rows, ]
  }
  form_list
}
#' @noRd
remove_records_from_project <- function(project, records) {
  id_col <- project$metadata$id_col
  if (length(records) == 0L) {
    cli_abort(
      paste0(
        "no records supplied to remove_records_from_project.",
        "Try {.code project$sync(hard_reset = TRUE)}"
      )
    )
  }
  project$data <- remove_from_form_list(form_list = project$data,
                                        id_col = id_col,
                                        records = records)
  project$transformation$data <- remove_from_form_list(
    form_list = project$transformation$data,
    id_col = id_col,
    records = records
  )
  keep_rows <- !project$record_summary[[id_col]] %in% records
  project$record_summary <- project$record_summary[which(keep_rows), ]
  invisible(project)
}
