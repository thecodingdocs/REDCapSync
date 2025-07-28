mock_project <- function() {
  short_name <- "TEST_PROJECT"
  redcap_uri <- "https://redcap.miami.edu/api/"
  project <- setup_project(
    short_name = short_name,
    redcap_uri = redcap_uri
  )
  project$internals$ever_connected <- TRUE
  fake_time <- now_time()
  project$internals$last_directory_save <- fake_time
  project$internals$last_metadata_update <- fake_time
  project$internals$last_data_update <- fake_time
  project$internals$last_sync <- fake_time
  project$internals$timezone <- Sys.timezone()
  project$redcap$version <- "12.1.1"
  project$redcap$token_name <- project$redcap$token_name
  project$redcap$project_id <- "01234"
  project$redcap$project_title <- "A Fake Project"
  project$redcap$id_col <- "record_id"
  project$redcap$is_longitudinal <- FALSE
  project$redcap$has_repeating_forms_or_events <- FALSE
  project$redcap$has_multiple_arms <- FALSE
  project$links$redcap_home <- project$links$redcap_uri
  project$links$redcap_API_playground <- project$links$redcap_uri
  return(project)
}
