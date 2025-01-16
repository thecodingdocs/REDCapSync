#' @title Syncronize your REDCaps
#' @param use_console Whether or not to use console to guide through sync vs just running
#' @param hard_reset Will go get all projects from scratch if TRUE.
#' @param project_names character vector of project short_names to check for sync.
#' Default is NULL which will check every project in cache.
#' @export
sync <- function(use_console = TRUE, hard_reset = FALSE,project_names = NULL) {
  collected <- makeAssertCollection()
  assert_logical(use_console, any.missing = FALSE, len = 1, add = collected)
  assert_logical(hard_reset, any.missing = FALSE, len = 1, add = collected)
  assert_character(hard_reset, any.missing = FALSE, add = collected)
  current_function <- as.character(current_call())[[1]]
  if( ! collected$isEmpty()){
    message <- collected %>% cli_message_maker(function_name = current_function)
    cli::cli_abort(message)
  }
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
    not_needed <- 0
    failed <- 0
    succeeded <- 0
    for(project_name in project_names){
      project_row <- which(projects$short_name == project_name)
      last_data_update <- projects$last_data_update
      project_status <- "Failed"
      PROJ <- load_project(short_name = project_name) # failure
      PROJ <- PROJ %>% sync_project(
        set_token_if_fails = use_console,
        save_to_dir = TRUE
        #other params
      )
      if(PROJ$internals$last_test_connection_outcome){
        project_status <- "Updated"
        if(PROJ$internals$last_data_update == last_data_update){
          project_status <- "Not Needed"
        }
      }
      projects$status[project_row] <- project_status
      rm(PROJ)
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  }
  not_needed <- sum(projects$status == "Not Needed")
  failed <- sum(projects$status == "Failed") # add why
  succeeded <- sum(projects$status == "Updated")
  not_failed <- succeeded + not_needed
  cli::cli_alert_info("{not_needed} REDCaps did not need syncing")
  cli::cli_alert_danger("{failed} REDCaps failed to sync")
  cli::cli_alert_success("{succeeded} REDCaps Updated!")
  cli::cli_alert_success("{not_failed} REDCaps Synced!")
  return(invisible())
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
