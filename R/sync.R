#' @title Syncronize your REDCaps
#' @export
sync <- function() {
  #interactive TRUE FALSE
  # projects <- get_projects()
  #?anyprojects - NO
  #if no setup whats home REDCap
  #setup first project
  #?anyprojects - Yes
  #sync_projects -----
  #loop
  #
  n <- 7
  cli::cli_progress_bar("Syncing REDCaps ...", total = 100)
  for (i in 1:100) {
    Sys.sleep(3/100)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  not_needed <- 1
  failed <- 1
  succeeded <- 3
  cli::cli_alert_info("{not_needed} REDCaps did not need syncing.")
  cli::cli_alert_danger("{failed} REDCaps failed to sync")
  cli::cli_alert_success("{succeeded} REDCaps Synced!")
}
#' @title Syncronize your REDCaps
#' @export
sync_projects <- function() {
  projects <- get_projects()
  #?anyprojects - NO
  #if no setup whats home REDCap
  #setup first project
  #?anyprojects - Yes
  #sync_projects -----
  #loop which ones

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
