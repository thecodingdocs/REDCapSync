#' @title Syncronize your REDCaps
#' @param use_console Whether or not to use console to guide through sync vs just running
#' @param hard_reset Will go get all projects from scratch if TRUE.
#' @param project_names character vector of project short_names to check for sync.
#' Default is NULL which will check every project in cache.
#' @export
sync <- function(
    use_console = TRUE,
    hard_reset = FALSE,
    project_names = NULL
) {
  collected <- makeAssertCollection()
  assert_logical(use_console, any.missing = FALSE, len = 1, add = collected)
  assert_logical(hard_reset, any.missing = FALSE, len = 1, add = collected)
  assert_character(
    project_names,
    null.ok = TRUE,
    any.missing = FALSE,
    add = collected
  )
  current_function <- as.character(current_call())[[1]]
  if( ! collected$isEmpty()){
    message <- collected %>% cli_message_maker(function_name = current_function)
    cli::cli_abort(message)
  }
  cli::boxx(
    "Start REDCapSync",
    padding = 1,
    background_col = "brown",
    float = "center"
  )
  #interactive TRUE FALSE
  projects <- get_projects()
  if(!is_something(projects)){
    # setup_project if interactive
    if(use_console){
      cli_abort("I can't help you with this yet. Dev.")
    }
  }else{
    if(is.null(project_names)) {
      project_names <- projects$short_name
    }
    # assert_choice(project_names) #placeholder
    project_names_length <- length(project_names)
    cli::cli_progress_bar("Syncing REDCaps ...", total = project_names_length)
    projects$status <- NA
    for(project_name in project_names){
      project_row <- which(projects$short_name == project_name)
      do_it <- due_for_sync(project_name)
      if(do_it){
        project_status <- "Failed"
        PROJ <- load_project(short_name = project_name) # failure tryCatch
        PROJ <- PROJ %>% sync_project(
          set_token_if_fails = use_console,
          save_to_dir = TRUE
          #other params
        )
        if(PROJ$internals$last_test_connection_outcome){
          project_status <- "Updated"
        }
      }else{
        cli::cli_alert_info("No need to update {project_name}: {then} ({sync_frequency})")
      }
      projects$status[project_row] <- project_status
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
    bullet_in_console("{short_name}: ", url = projects$redcap_home[project_row])
  }
  not_needed <- sum(projects$status == "Not Needed")
  failed <- sum(projects$status == "Failed") # add why
  succeeded <- sum(projects$status == "Updated")
  not_failed <- succeeded + not_needed
  cli::cli_alert_info("{not_needed} REDCaps did not need syncing")
  cli::cli_alert_danger("{failed} REDCaps failed to sync")
  cli::cli_alert_success("{succeeded} REDCaps Updated!")
  cli::cli_alert_success("{not_failed} REDCaps Synced!")
  print.table(projects[,c("short_name","sync_frequency","status")])
  cli::boxx(
    "End REDCapSync",
    padding = 1,
    background_col = "brown",
    float = "center"
  )
  return(invisible())
}
due_for_sync<- function (project_name) {
  now <- Sys.time()
  projects <- get_projects()
  #early escapes ----
  checkmate::assert_data_frame(projects,min.rows = 1)
  assert_names(projects$short_name, must.include = project_name)
  #-----
  project_row <- which(projects$short_name == project_name)
  last_data_update <- projects$last_data_update[project_row]
  if(is.na(last_data_update)){
    return(TRUE)
  }
  then <- as.POSIXct(last_data_update, format = "%Y-%m-%d %H:%M:%OS",tz = Sys.timezone())
  sync_frequency <- projects$sync_frequency[project_row]
  if(sync_frequency == "always"){
    return(TRUE)
  }
  have_to_check <- sync_frequency %in%c("hourly", "daily", "weekly", "monthly")
  if(have_to_check){ # turn to function
    if(sync_frequency == "hourly"){
      do_it <- now >= (then + lubridate::dhours(1))
    }
    if(sync_frequency == "daily"){
      do_it <- now >= (then + lubridate::ddays(1))
    }
    if(sync_frequency == "weekly"){
      do_it <- now >= (then + lubridate::dweeks(1))
    }
    if(sync_frequency == "monthly"){
      do_it <- now >= (then + lubridate::dmonths(1))
    }
  }
  return(do_it)
}
due_for_sync2 <- function(){
  now <- Sys.time()
  projects <- get_projects()
  if(nrow(projects)==0){
    return(NULL)
  }
  project_names <- projects$short_name
  # Early escapes ----
  checkmate::assert_data_frame(projects, min.rows = 1)
  assert_names(projects$short_name, must.include = project_names)
  # Prepare results
  results <- logical(length(project_names))

  results_check <- which(
    !is.na(projects$last_data_update) |
      ! projects$sync_frequency %in% c("always","never")
  )
  results_no_check_true <- which(
    is.na(projects$last_data_update) |
                                   is.na(projects$sync_frequency) |
                                   projects$sync_frequency == "always"
                                 )
  results_no_check_true <- which(projects$sync_frequency == "never")
    #
    # for (i in seq_along(project_names)) {
    #   project_name <- project_names[i]
    #   project_row <- which(projects$short_name == project_name)
    #   last_data_update <- projects$last_data_update[project_row]
    #   if (is.na(last_data_update)) {
    #     results[i] <- TRUE
    #     next
    #   }
    #   then <- as.POSIXct(last_data_update, format = "%Y-%m-%d %H:%M:%OS", tz = Sys.timezone())
    #   sync_frequency <- projects$sync_frequency[project_row]
    #
    #   if (sync_frequency == "always") {
    #     results[i] <- TRUE
    #     next
    #   }
    #
    #   if (sync_frequency %in% c("hourly", "daily", "weekly", "monthly")) {
    #     if (sync_frequency == "hourly") {
    #       results[i] <- now >= (then + lubridate::dhours(1))
    #     } else if (sync_frequency == "daily") {
    #       results[i] <- now >= (then + lubridate::ddays(1))
    #     } else if (sync_frequency == "weekly") {
    #       results[i] <- now >= (then + lubridate::dweeks(1))
    #     } else if (sync_frequency == "monthly") {
    #       results[i] <- now >= (then + lubridate::dmonths(1))
    #     }
    #   } else {
    #     results[i] <- FALSE
    #   }
    # }
    #
  return(results)
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
  #     OUT <- NULL
  #     OUT <- projects_old[i,]
  #     if(file.exists(OUT$dir_path)){
  #       OUT$test_dir <- TRUE
  #       project <- tryCatch({
  #         load_project(OUT$dir_path)
  #       },error = function(e) {NULL})
  #       OUT$test_project <- !is.null(project)
  #       if(!OUT$test_project){
  #         project <- tryCatch({
  #           setup_project(
  #             short_name = OUT$short_name,
  #             dir_path = OUT$dir_path,
  #             token_name = OUT$token_name,
  #             redcap_base = "https://redcap.miami.edu/",
  #             reset = TRUE,
  #             merge_form_name = "merged"
  #           )
  #         },error = function(e) {NULL})
  #         OUT$test_project <- !is.null(project)
  #       }
  #       if(OUT$test_project){
  #         OUT$test_RC <- redcap_token_works(project)
  #         if(OUT$test_RC){
  #           if(update)project <- sync_project(project)
  #         }
  #       }
  #       if(OUT$test_project){
  #         OUT_project <- extract_project_details(project = project)
  #         OUT_project$test_dir <- OUT$test_dir
  #         OUT_project$test_project <- OUT$test_project
  #         OUT_project$test_RC <- OUT$test_RC
  #         OUT <- OUT_project
  #       }
  #       projects <- projects[which(projects$short_name!=OUT$short_name),]
  #       projects <- projects %>% dplyr::bind_rows(OUT)
  #     }
  #   }
  #   save_projects_to_cache(projects,silent = FALSE)
  # }
}
