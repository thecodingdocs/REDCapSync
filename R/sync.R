#' @title Synchronize your REDCaps
#' @param project_names character vector of project short_names to check for
#' sync. Default is NULL which will check every project in cache.
#' @inheritParams sync_project
#' @param hard_reset Will go get all projects from scratch if TRUE.
#' @export
sync <- function(
    project_names = NULL,
    summarize = TRUE,
    hard_check = FALSE,
    hard_reset = FALSE
    ) {
  collected <- makeAssertCollection()
  assert_logical(hard_reset, any.missing = FALSE, len = 1, add = collected)
  assert_character(
    project_names,
    null.ok = TRUE,
    any.missing = FALSE,
    add = collected
  )
  current_function <- as.character(current_call())[[1]]
  if (!collected$isEmpty()) {
    message <- collected %>% cli_message_maker(function_name = current_function)
    cli::cli_abort(message)
  }
  cli::cli_h1("Starting")
  cli::cat_boxx(
    "REDCapSync",
    padding = 1,
    background_col = "brown"
  )
  sweep_dirs_for_cache() # will overwrite new settings?
  # interactive TRUE FALSE
  projects <- get_projects()
  if (!is_something(projects)) {
    # setup_project if interactive
    cli_abort("You have not setup any projects yet! Use `setup_project()`")
    return(invisible())
  }
  if (is.null(project_names)) {
    project_names <- projects$short_name
  }
  # assert_choice(project_names) #placeholder
  project_names_length <- length(project_names)
  cli::cli_progress_bar("Syncing REDCaps ...", total = project_names_length)
  projects$status <- NA
  # vector_of_due <- due_for_sync2()
  project_list <- projects %>% split(projects$short_name)
  # are_due <- due_for_sync2()
  for (project_name in names(project_list)) {
    project_details <- project_list[[project_name]]
    project_row <- which(projects$short_name == project_name)
    project_status <- "Not Needed"
    # what determines due if setting changed?
    # add to due for sync?
    project <- setup_project(
      short_name = project_details$short_name,
      dir_path = project_details$dir_path,
      redcap_base = project_details$redcap_base,
      token_name = project_details$token_name,
      sync_frequency = project_details$sync_frequency,
      get_type = project_details$get_type,
      labelled = project_details$labelled,
      metadata_only = project_details$metadata_only,
      batch_size_download = project_details$batch_size_download,
      batch_size_upload = project_details$batch_size_upload,
      entire_log = project_details$entire_log,
      days_of_log = project_details$days_of_log,
      get_files = project_details$get_files,
      get_file_repository = project_details$get_file_repository,
      original_file_names = project_details$original_file_names,
      merge_form_name = project_details$merge_form_name,
      use_csv = project_details$use_csv,
      hard_reset = hard_reset
    )
    original_last_sync <- project$internals$last_sync
    original_last_summary <- project$internals$last_summary
    project <- tryCatch(
      expr = {
        suppressWarnings({
          list(
            project = project %>% sync_project(
              hard_reset = hard_reset,
              hard_check = hard_check,
              summarize = summarize
            ),
            sync_failed = FALSE
          )
        })
      },
      error = function(e) {
        message("Sync failed: ", e$message)
        list(
          project = project,
          sync_failed = TRUE
        )
      }
    )
    sync_failed <- project$sync_failed
    project <- project$project
    if (sync_failed) {
      project_status <- "Failed"
    } else {
      synced_last_sync <- project$internals$last_sync
      synced_last_summary <- project$internals$last_summary
      if(!identical(original_last_sync,synced_last_sync)){
        project_status <- "Updated"
      }
      if(!identical(original_last_summary,synced_last_summary)){
        project_status <- "Updated"
      }
    }
    projects$status[project_row] <- project_status
    the_link <- projects$redcap_home[project_row]
    if (is_something(the_link)) {
      cli_alert_wrap(paste0(project_name, ": "), url = the_link)
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  not_needed <- sum(projects$status == "Not Needed")
  failed <- sum(projects$status == "Failed") # add why
  succeeded <- sum(projects$status == "Updated")
  not_failed <- succeeded + not_needed
  print(projects[, c("short_name", "sync_frequency", "status")])
  if (not_needed > 0) {
    cli::cli_alert_info("{not_needed} REDCaps did not need syncing")
  }
  if (failed > 0) {
    cli::cli_alert_danger("{failed} REDCaps failed to sync")
  }
  if (succeeded > 0) {
    cli::cli_alert_success("{succeeded} REDCaps Updated!")
  }
  if (not_failed > 0) {
    cli::cli_alert_success("{not_failed} REDCaps Synced!")
  }
  cli::cli_h1("Done!")
  invisible()
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
due_for_sync2 <- function() {
  now <- now_time()
  projects <- get_projects()
  if (nrow(projects) == 0) {
    return(NULL)
  }
  project_names <- projects$short_name
  # Early escapes ----
  assert_data_frame(projects, min.rows = 1)
  assert_names(projects$short_name, must.include = project_names)
  # Prepare results
  results <- logical(length(project_names))
  results_check <- which(
    !is.na(projects$last_data_update) &
      !projects$sync_frequency %in% c("always", "never")
  )
  results_no_check_true <- which(
    is.na(projects$last_data_update) |
      is.na(projects$sync_frequency) |
      projects$sync_frequency == "always"
  )
  results_no_check_false <- which(projects$sync_frequency == "never")
  results[results_no_check_true] <- TRUE
  results[results_no_check_false] <- FALSE
  if (length(results_check) > 0) {
    then <- as.POSIXct(projects$last_data_update[results_check],
                       format = "%Y-%m-%d %H:%M:%OS",
                       tz = Sys.timezone())
    sync_frequency <- projects$sync_frequency[results_check]
    time_diff_map <- list(
      hourly = lubridate::dhours(1),
      daily = lubridate::ddays(1),
      weekly = lubridate::dweeks(1),
      monthly = lubridate::dmonths(1)
    )
    results[results_check] <- mapply(function(last_update, freq) {
      if (!freq %in% names(time_diff_map)) {
        return(FALSE)
      }
      now >= (last_update + time_diff_map[[freq]])
    }, then, sync_frequency, SIMPLIFY = TRUE)
  }
  result_rows <- which(results)
  if (length(result_rows) == 0) {
    return(NULL)
  }
  project_names[result_rows]
}
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
      expected_path <- file.path(
        from_cache$dir_path,
        "R_objects",
        paste0(project_name, .project_details_path_suffix)
      ) %>% sanitize_path()
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
          # assert_project_details(project_details)
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
#' @title project_health_check
#' @description
#' Check directory, project object, and REDCap token. Optional update.
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @return project cache data.frame
#' @export
project_health_check <- function() {
  # projects <- projects_old <- get_projects()
  # DROPS <- NULL
  # projects_old$test_dir <- FALSE
  # projects$test_project <- FALSE
  # projects$test_RC <- FALSE
  # if(nrow(projects)>0){
  #   # DROPS<- projects[which(is.na(projects$project_id)),]
  #   for(i in seq_len(nrow(projects_old))){#i <- seq_len(nrow(projects))%>%  sample1()
  #     form <- NULL
  #     form <- projects_old[i,]
  #     if(file.exists(form$dir_path)){
  #       form$test_dir <- TRUE
  #       project <- tryCatch({
  #         load_project(form$dir_path)
  #       },error = function(e) {NULL})
  #       form$test_project <- !is.null(project)
  #       if(!form$test_project){
  #         project <- tryCatch({
  #           setup_project(
  #             short_name = form$short_name,
  #             dir_path = form$dir_path,
  #             token_name = form$token_name,
  #             redcap_base = "https://redcap.miami.edu/",
  #             hard_reset = TRUE,
  #             merge_form_name = "merged"
  #           )
  #         },error = function(e) {NULL})
  #         form$test_project <- !is.null(project)
  #       }
  #       if(form$test_project){
  #         form$test_RC <- redcap_token_works(project)
  #         if(form$test_RC){
  #           if(update)project <- sync_project(project)
  #         }
  #       }
  #       if(form$test_project){
  #         form_project <- extract_project_details(project = project)
  #         form_project$test_dir <- form$test_dir
  #         form_project$test_project <- form$test_project
  #         form_project$test_RC <- form$test_RC
  #         form <- form_project
  #       }
  #       projects <- projects[which(projects$short_name!=form$short_name),]
  #       projects <- projects %>% dplyr::bind_rows(form)
  #     }
  #   }
  #   save_projects_to_cache(projects,silent = FALSE)
  # }
}
