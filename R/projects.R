#' @title Get your REDCap projects used by REDCapSync
#' @description
#' Everytime a setup or update is performed REDCapSync stores the most basic
#' information
#' about that project to the cache so the user has a running log of everywhere
#' there project information is stored,
#' which can be used to find, move, edit, delete that data.
#' @return data.frame of projects from the cache
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @export
get_projects <- function() {
  does_exist <- cache_projects_exists()
  is_ok <- FALSE
  if (does_exist) {
    projects <- cache_path() %>%
      file.path("projects.rds") %>%
      readRDS()
    if (!does_exist) {
      message("You have no projects cached. Try `setup_project()`")
    }
    is_ok <- all(colnames(.blank_project_details %in% colnames(projects)))
    if (!is_ok)
      cache_clear()
  }
  if (!does_exist || !is_ok) {
    return(.blank_project_details)
  }
  projects
}
#' @noRd
blank_tibble <- function(col_names){
  if(missing(col_names)) {
    return(tibble())
  }
  checkmate::assert_vector(col_names, unique = TRUE)
  df <- matrix(data = character(0),ncol = length(col_names))
  colnames(df) <- col_names
  as_tibble(df)
}
#' @noRd
blank_data_frame <- function(col_names){
  if(missing(col_names)) {
    return(data.frame())
  }
  checkmate::assert_vector(col_names, unique = TRUE)
  df <- matrix(data = character(0),ncol = length(col_names))
  colnames(df) <- col_names
  as.data.frame(df)
}
#' @noRd
.blank_project_cols <- c(
  "short_name",
  "dir_path",
  "sync_frequency",
  "last_sync",
  "timezone",
  "last_directory_save",
  "last_metadata_update",
  "last_data_update",
  "version",
  "token_name",
  "project_id",
  "project_title",
  "labelled",
  "id_col",
  "is_longitudinal",
  "has_repeating_forms_or_events",
  "has_multiple_arms",
  "R_object_size",
  "file_size",
  "n_records",
  "redcap_uri",
  "redcap_base",
  "redcap_home",
  "redcap_api_playground",
  "days_of_log",
  "get_files",
  "get_file_repository",
  "original_file_names",
  "entire_log",
  "metadata_only",
  "add_default_fields",
  "add_default_transformation",
  "add_default_summaries",
  "use_csv",
  "get_type",
  "batch_size_download",
  "batch_size_upload"
)
#' @noRd
.blank_project_details <- data.frame(
  short_name = character(0),
  dir_path = character(0),
  sync_frequency = character(0),
  last_sync = character(0) %>% as.POSIXct(tz = Sys.timezone()),
  timezone = character(0),
  last_directory_save = character(0) %>% as.POSIXct(tz = Sys.timezone()),
  last_metadata_update = character(0) %>% as.POSIXct(tz = Sys.timezone()),
  last_data_update = character(0) %>% as.POSIXct(tz = Sys.timezone()),
  version = character(0),
  token_name = character(0),
  project_id = character(0),
  project_title = character(0),
  labelled = logical(0),
  id_col = character(0),
  is_longitudinal = logical(0),
  has_repeating_forms_or_events = logical(0),
  has_multiple_arms = logical(0),
  R_object_size = numeric(0),
  file_size = numeric(0),
  n_records = integer(0),
  redcap_uri = character(0),
  redcap_base = character(0),
  redcap_home = character(0),
  redcap_api_playground = character(0),
  days_of_log = integer(0),
  get_files = logical(0),
  get_file_repository = logical(0),
  original_file_names = logical(0),
  entire_log = logical(0),
  metadata_only = logical(0),
  add_default_fields = logical(0),
  add_default_transformation = logical(0),
  add_default_summaries = logical(0),
  use_csv = logical(0),
  get_type = character(0),
  batch_size_download = integer(0),
  batch_size_upload = integer(0),
  stringsAsFactors = FALSE
)
#' @noRd
save_projects_to_cache <- function(projects, silent = TRUE) {
  assert_project_details(projects)
  projects <- projects[order(projects$short_name), ]
  saveRDS(projects, file = cache_path() %>% file.path("projects.rds"))
  pkg_name <- "REDCapSync"
  if (!silent) {
    cli_alert_success(
      paste0(
        pkg_name,
        " saved ",
        nrow(projects),
        " project locations to the cache...",
        toString(projects$short_name)
      ) # "   Token: ",projects$token_name,collapse = "\n"))
    )
    cli_alert_wrap(
      text = paste0(
        "Cache is stored in directory on your computer. It can be found with `",
        pkg_name,
        "::cache_path()`, and cleared with `",
        pkg_name,
        "::cache_clear()`."
      ),
      file = cache_path()
    )
  }
}
#' @noRd
na_if_null <- function(x) {
  ifelse(is.null(x), NA, x)
}
#' @noRd
extract_project_details <- function(project) {
  assert_setup_project(project)
  project_details <- matrix(
    data = NA,
    ncol = length(.blank_project_cols),
    nrow = 1,
    dimnames = list(NULL, .blank_project_cols)
  ) %>% as.data.frame()
  # top -----
  project_details$short_name <- project$short_name
  project_details$dir_path <- project$dir_path %>% na_if_null()
  # settings -------
  project_details$sync_frequency <- project$internals$sync_frequency
  project_details$days_of_log <- project$internals$days_of_log %>% as.integer()
  project_details$get_files <- project$internals$get_files
  project_details$get_file_repository <- project$internals$get_file_repository
  project_details$original_file_names <- project$internals$original_file_names
  project_details$entire_log <- project$internals$entire_log
  project_details$metadata_only <- project$internals$metadata_only
  project_details$add_default_fields <-
    project$internals$add_default_fields
  project_details$add_default_transformation <-
    project$internals$add_default_transformation
  project_details$add_default_summaries <-
    project$internals$add_default_summaries
  project_details$use_csv <- project$internals$use_csv
  project_details$labelled <- project$internals$labelled
  project_details$get_type <- project$internals$get_type
  project_details$batch_size_download <-
    project$internals$batch_size_download %>%
    as.integer()
  project_details$batch_size_upload <-
    project$internals$batch_size_upload %>%
    as.integer()
  # redcap --------
  project_details$version <- project$redcap$version %>% na_if_null()
  project_details$token_name <- project$redcap$token_name %>% na_if_null()
  project_details$project_id <- project$redcap$project_id %>% na_if_null()
  project_details$project_title <- project$redcap$project_title %>% na_if_null()
  project_details$id_col <- project$metadata$id_col %>% na_if_null()
  project_details$is_longitudinal <- project$metadata$is_longitudinal %>%
    na_if_null()
  project_details$has_repeating_forms_or_events <-
    project$metadata$has_repeating_forms_or_events %>%
    na_if_null()
  project_details$has_multiple_arms <-
    project$metadata$has_multiple_arms %>% na_if_null()
  project_details$n_records <-
    length(project$summary$all_records[[project$metadata$id_col]]) %>%
    na_if_null() %>%
    as.integer()
  project_details$redcap_uri <- project$links$redcap_uri %>% na_if_null()
  project_details$redcap_base <- project$links$redcap_base %>% na_if_null()
  project_details$redcap_home <- project$links$redcap_home %>% na_if_null()
  project_details$redcap_api_playground <-
    project$links$redcap_api_playground %>%
    na_if_null()
  # saving ----
  project_details$timezone <- project$internals$timezone %>% na_if_null()
  project_details$last_sync <- project$internals$last_sync %>%
    na_if_null() %>%
    as.POSIXct(tz = Sys.timezone())
  project_details$last_directory_save <-
    project$internals$last_directory_save %>%
    na_if_null() %>%
    as.POSIXct(tz = Sys.timezone())
  project_details$last_metadata_update <-
    project$internals$last_metadata_update %>%
    na_if_null() %>%
    as.POSIXct(tz = Sys.timezone())
  project_details$last_data_update <- project$internals$last_data_update %>%
    na_if_null() %>%
    as.POSIXct(tz = Sys.timezone())
  project_details$R_object_size <- NA
  project_details$file_size <- NA
  project_details
}
#' @noRd
add_project_details_to_cache <- function(project_details) {
  assert_project_details(project_details, nrows = 1)
  projects <- get_projects()
  projects <- projects[which(projects$short_name != project_details$short_name), ]
  bad_row <- NULL
  if(!is.na(project_details$project_id)){
    bad_row <- which(
      projects$project_id == project_details$project_id &
        dirname(projects$redcap_uri) == dirname(project_details$redcap_uri)
    )
  }
  if (length(bad_row) > 0) {
    cli::cli_abort(
      paste0(
        "You are trying to save from a project [{project_details$short_name} ",
        "PID {projects$project_id[bad_row]}] that you have already setup ",
        "[{projects$short_name[bad_row]} PID {project_details$project_id}] ",
        "You can load the old project or run ",
        "`cache_remove_project(\"{projects$short_name[bad_row]}\")`"
      )
    )
  }
  projects <- projects %>% bind_rows(project_details)
  save_projects_to_cache(projects)
}
#' @noRd
add_project_details_to_project <- function(project, project_details) {
  assert_setup_project(project)
  assert_project_details(project_details, nrows = 1)
  # compare_project_details()
  # top -----
  if (!identical(project$short_name, project_details$short_name)) {
    stop("project and project_details short_name must be identical!")
  }
  # if(!identical(project$dir_path, project_details$dir_path)){
  #   stop("project and project_details short_name must be identical!")
  # }
  # project$dir_path <- project_details$dir_path
  # settings -------
  project$internals$sync_frequency <- project_details$sync_frequency
  project$internals$days_of_log <- project_details$days_of_log %>% as.integer()
  project$internals$get_files <- project_details$get_files
  project$internals$get_file_repository <- project_details$get_file_repository
  project$internals$original_file_names <- project_details$original_file_names
  project$internals$entire_log <- project_details$entire_log
  project$internals$metadata_only <- project_details$metadata_only
  project$internals$add_default_fields <-
    project_details$add_default_fields
  project$internals$add_default_transformation <-
    project_details$add_default_transformation
  project$internals$add_default_summaries <-
    project_details$add_default_summaries
  project$internals$use_csv <- project_details$use_csv
  project$internals$get_type <- project_details$get_type # should trigger hard_reset
  project$internals$batch_size_download <-
    project_details$batch_size_download %>%
    as.integer()
  project$internals$batch_size_upload <- project_details$batch_size_upload %>%
    as.integer()
  # redcap --------
  # project$redcap$version <- project_details$version # check identical unless NA
  # project$redcap$token_name <- project_details$token_name
  # if (!is.na(project$redcap$project_id)) {
  #   project$redcap$project_id <- project_details$project_id
  # }
  # project$redcap$project_title <- project_details$project_title
  # project$metadata$id_col <- project_details$id_col # check identical unless NA
  # project$metadata$is_longitudinal <- project_details$is_longitudinal
  # project$metadata$has_repeating_forms_or_events <- project_details$has_repeating_forms_or_events
  # project$metadata$has_multiple_arms <- project_details$has_multiple_arms
  # project$summary$all_records[[project$metadata$id_col]] <- project_details$n_records %>% as.integer()
  # project$links$redcap_uri <- project_details$redcap_uri # check identical unless NA
  # project$links$redcap_home <- project_details$redcap_home
  # project$links$redcap_api_playground <- project_details$redcap_api_playground
  # saving ----
  project$internals$timezone <- project_details$timezone
  project$internals$last_sync <- project_details$last_sync %>%
    as.POSIXct(tz = Sys.timezone())
  project$internals$last_directory_save <- project_details$last_directory_save %>%
    as.POSIXct(tz = Sys.timezone())
  project$internals$last_metadata_update <- project_details$last_metadata_update %>%
    as.POSIXct(tz = Sys.timezone())
  project$internals$last_data_update <- project_details$last_data_update %>%
    as.POSIXct(tz = Sys.timezone())
  # project_details$R_object_size <- NA
  # project_details$file_size <- NA
  # bad_row <- which(
  #   projects$project_id == project_details$project_id &
  #     basename(projects$redcap_uri) == basename(project_details$redcap_uri)
  # )
  # if(length(bad_row)>0) {
  #   cli::cli_abort(
  #     paste0(
  #       "You are trying to save from a project [{project_details$short_name} PID ",
  #       "{projects$project_id[bad_row]}] that you have already setup ",
  #       "[{projects$short_name[bad_row]} PID {project_details$project_id}] ",
  #       "You can load the old project or run ",
  #       "`delete_project_by_name(\"{projects$short_name[bad_row]}\")`"
  #     )
  #   )
  # }
  invisible(project)
}
#' @noRd
save_project_details <- function(project, silent = TRUE) {
  assert_setup_project(project)
  assert_logical(silent, len = 1)
  project_details <- extract_project_details(project)
  add_project_details_to_cache(project_details)
  save_project_details_path <- get_project_path2(project, type = "details")
  current_function <- as.character(current_call())[[1]]
  if (is_something(save_project_details_path)) {
    if (file.exists(save_project_details_path)) {
      to <- readRDS(save_project_details_path)
      from <- project_details
      collected <- makeAssertCollection()
      assert_set_equal(from$short_name, to$short_name, add = collected)
      if (!is.na(from$project_id) && !is.na(to$project_id)) {
        assert_set_equal(from$project_id, to$project_id, add = collected)
      }
      if (!is.na(from$redcap_uri) && !is.na(to$redcap_uri)) {
        assert_set_equal(from$redcap_uri, to$redcap_uri, add = collected)
      }
      if (!collected$isEmpty()) {
        info <- "Something critical doesn't match. You should run `delete_project_by_name(\"{project$short_name}\")"
        collected %>%
          cli_message_maker(function_name = current_function, info = info) %>%
          cli::cli_abort()
      }
      if (!check_set_equal(from$dir_path, to$dir_path)) {
        cli::cli_alert_warning(
          "Cache dir doesn't match save dir. You should only see this if you syncing this project from cloud directories where the paths vary."
        )
      }
    }
    saveRDS(object = project_details, file = save_project_details_path) # add error check
  }
  return(invisible())
}
#' @noRd
.arms_colnames <- c(
  "arm_number",
  "arm_name"
)
#' @noRd
.events_colnames <- c(
  "event_name",
  "arm_number",
  "unique_event_name",
  "custom_event_label",
  "event_id"
)
#' @noRd
.event_mapping_colnames <- c(
  "arm_num",
  "unique_event_name",
  "form"
)
#' @noRd
.forms_colnames <- c(
  "form_name",
  "form_label"
)
.choices_colnames <- c(
  "field_name",
  "code",
  "name"
)
#' @noRd
.project_info_colnames <- c(
  "project_id",
  "project_title",
  "creation_time",
  "production_time",
  "in_production",
  "project_language",
  "purpose",
  "purpose_other",
  "project_notes",
  "custom_record_label",
  "secondary_unique_field",
  "is_longitudinal",
  "has_repeating_instruments_or_events",
  "surveys_enabled",
  "scheduling_enabled",
  "record_autonumbering_enabled",
  "randomization_enabled",
  "ddp_enabled",
  "project_irb_number",
  "project_grant_number",
  "project_pi_firstname",
  "project_pi_lastname",
  "display_today_now_button",
  "missing_data_codes",
  "external_modules",
  "bypass_branching_erase_field_prompt"
)
#' @noRd
.log_colnames <- c(
  "timestamp",
  "username",
  "action",
  "details",
  "record",
  "action_type"
)
#' @noRd
.users_colnames <- c(
  "username",
  "email",
  "firstname",
  "lastname",
  "expiration",
  "data_access_group",
  "data_access_group_id",
  "design",
  "alerts",
  "user_rights",
  "data_access_groups",
  "reports",
  "stats_and_charts",
  "manage_survey_participants",
  "calendar",
  "data_import_tool",
  "data_comparison_tool",
  "logging",
  "file_repository",
  "data_quality_create",
  "data_quality_execute",
  "api_export",
  "api_import",
  "api_modules",
  "mobile_app",
  "mobile_app_download_data",
  "record_create",
  "record_rename",
  "record_delete",
  "lock_records_all_forms",
  "lock_records",
  "lock_records_customization",
  "forms_export"
)
#' @noRd
.field_colnames <- c(
  "field_name",
  "form_name",
  "section_header",
  "field_type",
  "field_label",
  "select_choices_or_calculations",
  "field_note",
  "text_validation_type_or_show_slider_number",
  "text_validation_min",
  "text_validation_max",
  "identifier",
  "branching_logic",
  "required_field",
  "custom_alignment",
  "question_number",
  "matrix_group_name",
  "matrix_ranking",
  "field_annotation"
)
