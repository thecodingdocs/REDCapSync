#' @title Get your REDCap projects used by REDCapSync
#' @description
#' Everytime a setup or update is performed REDCapSync stores the most basic information
#' about that project to the cache so the user has a running log of everywhere there project information is stored,
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
    if (!does_exist) message("You have no projects cached. Try `setup_project()`")
    is_ok <- all(colnames(blank_project() %in% colnames(projects)))
    if (!is_ok) cache_clear()
  }
  if (!does_exist || !is_ok) {
    return(blank_project())
  }
  return(projects)
}
#' @title List File Paths of REDCapSync Projects in a Folder
#' @description
#' Searches a specified folder for files related to REDCapSync projects and returns their file paths.
#' Optionally validates the folder to ensure it was previously set up using `setup_project()`.
#'
#' @param file_path Character. The path to the folder to search.
#' @param validate Logical. If `TRUE`, the function will only accept valid directories previously set up with `setup_project()`. Default is `TRUE`.
#'
#' @return
#' A character vector of file paths for valid REDCapSync project files in the folder. Returns an empty character vector if no valid files are found.
#'
#' @details
#' This function checks a folder (and optionally validates its setup) for `.RData` files that correspond to REDCapSync projects.
#' It identifies files with the extension `.RData` and names ending in `_REDCapSync`, filtering out any unrelated files.
#'
#' @seealso
#' \link{setup_project} for setting up valid directories.
#'
#' @export
check_folder_for_projects <- function(file_path, validate = TRUE) {
  check_path <- file_path
  if (validate) {
    file_path <- assert_dir(file_path)
    check_path <- file.path(file_path, "R_objects")
  }
  files <- list.files.real(check_path, full.names = TRUE, recursive = TRUE)
  if (length(file) == 0) {
    return(character(0))
  }
  file_name <- tools::file_path_sans_ext(basename(files))
  file_ext <- tools::file_ext(files)
  df <- data.frame(
    file_path = files,
    file_name = file_name,
    file_ext = file_ext
  )
  df <- df[which((df$file_ext == "RData") & (endsWith(df$file_name, "_REDCapSync"))), ]
  if (nrow(df) == 0) {
    return(character(0))
  }
  return(df$file_path)
}
#' @noRd
internal_blank_project_cols <- c(
  # core ------
  "short_name",
  "dir_path",
  "sync_frequency",
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
  "redcap_base",
  "redcap_home",
  "redcap_API_playground",
  "days_of_log",
  "get_files",
  "get_file_repository",
  "original_file_names",
  "entire_log",
  "metadata_only",
  "merge_form_name",
  "use_csv",
  "get_type",
  "batch_size_download",
  "batch_size_upload"
  # "test_dir"
  # "test_project",
  # "test_RC"
)
#' @noRd
blank_project <- function() {
  x <- data.frame(
    # core -----
    short_name = character(0),
    dir_path = character(0),
    # settings -----
    sync_frequency = character(0),
    last_directory_save = character(0) %>% as.POSIXct(),
    last_metadata_update = character(0)%>% as.POSIXct(),
    last_data_update = character(0)%>% as.POSIXct(),
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
    redcap_base = character(0),
    redcap_home = character(0),
    redcap_API_playground = character(0),
    days_of_log = integer(0),
    get_files = logical(0),
    get_file_repository = logical(0),
    original_file_names = logical(0),
    entire_log = logical(0),
    metadata_only = logical(0),
    merge_form_name = character(0),
    use_csv = logical(0),
    get_type = character(0),
    batch_size_download = integer(0),
    batch_size_upload = integer(0)
  )
  # assert_names(
  #   internal_blank_project_cols
  # )
  return(x)
}
#' @noRd
save_projects_to_cache <- function(projects, silent = TRUE) {
  projects <- projects[order(projects$short_name), ]
  saveRDS(projects, file = cache_path() %>% file.path("projects.rds"))
  if (!silent) {
    bullet_in_console(
      bullet_type = "v",
      text = paste0(
        pkg_name,
        " saved ",
        nrow(projects),
        " project locations to the cache...",
        paste0(projects$short_name, collapse = ", ")
      ) # "   Token: ",projects$token_name,collapse = "\n"))
    )
    bullet_in_console(
      text = paste0(
        "The cache is stored in directory on your computer. It can be found with `",
        pkg_name,
        "::cache_path()`, and cleared with `",
        pkg_name,
        "::cache_clear()`."
      ),
      file = cache_path()
    )
  }
}
na_if_null <- function(x){
  return(ifelse(is.null(x),NA,x))
}
#' @noRd
extract_project_details <- function(project) {
  assert_setup_project(project)
  OUT <- matrix(
    data = NA,
    ncol = length(internal_blank_project_cols),
    nrow = 1,
    dimnames = list(
      NULL,
      internal_blank_project_cols
    )
  ) %>% as.data.frame()
  #top -----
  OUT$short_name <- project$short_name
  OUT$dir_path <- project$dir_path %>% na_if_null()
  # settings -------
  OUT$sync_frequency <- project$internals$sync_frequency
  OUT$days_of_log <- project$internals$days_of_log %>% as.integer()
  OUT$get_files <- project$internals$get_files
  OUT$get_file_repository <- project$internals$get_file_repository
  OUT$original_file_names <- project$internals$original_file_names
  OUT$entire_log <- project$internals$entire_log
  OUT$metadata_only <- project$internals$metadata_only
  OUT$use_csv <- project$internals$use_csv
  OUT$get_type <- project$internals$get_type
  OUT$labelled <- project$internals$labelled
  OUT$merge_form_name <- project$internals$merge_form_name
  OUT$batch_size_download <- project$internals$batch_size_download %>% as.integer()
  OUT$batch_size_upload <- project$internals$batch_size_upload %>% as.integer()
  # redcap --------
  OUT$version <-  project$redcap$version %>% na_if_null()
  OUT$token_name <- project$redcap$token_name %>% na_if_null()
  OUT$project_id <-  project$redcap$project_id %>% na_if_null()
  OUT$project_title <-  project$redcap$project_title %>% na_if_null()
  OUT$id_col <-  project$redcap$id_col %>% na_if_null()
  OUT$is_longitudinal <-  project$redcap$is_longitudinal %>% na_if_null()
  OUT$has_repeating_forms_or_events <-  project$redcap$has_repeating_forms_or_events %>% na_if_null()
  OUT$has_multiple_arms <-  project$redcap$has_multiple_arms %>% na_if_null()
  OUT$n_records <-  length(project$summary$all_records[[project$redcap$id_col]]) %>% na_if_null() %>% as.integer()
  OUT$redcap_base <-  project$links$redcap_base %>% na_if_null()
  OUT$redcap_home <-  project$links$redcap_home %>% na_if_null()
  OUT$redcap_API_playground <-  project$links$redcap_API_playground %>% na_if_null()
  # saving ----
  OUT$last_directory_save <- project$internals$last_directory_save %>% na_if_null() %>% as.POSIXct()
  OUT$last_metadata_update <- project$internals$last_metadata_update %>% na_if_null() %>% as.POSIXct()
  OUT$last_data_update <- project$internals$last_data_update %>% na_if_null() %>% as.POSIXct()
  OUT$R_object_size <- NA
  OUT$file_size <- NA
  return(OUT)
}
#' @noRd
add_project_to_cache <- function(project, silent = TRUE) {
  assert_setup_project(project)
  assert_logical(silent, len = 1)
  projects <- get_projects()
  projects <- projects[which(projects$short_name != project$short_name), ]
  OUT <- extract_project_details(project = project)
  bad_row <- which(
    projects$project_id == OUT$project_id &
      basename(projects$redcap_base) == basename(OUT$redcap_base)
  )
  if(length(bad_row)>0) {
    cli::cli_abort(
      paste0(
        "You are trying to save from a project [{OUT$short_name} PID ",
        "{projects$project_id[bad_row]}] that you have already setup ",
        "[{projects$short_name[bad_row]} PID {OUT$project_id}] ",
        "You can load the old project or run ",
        "`delete_project_by_name(\"{projects$short_name[bad_row]}\")`"
      )
    )
  }
  projects <- projects %>% dplyr::bind_rows(OUT)
  save_projects_to_cache(projects, silent = silent)
}
#' @noRd
delete_project_by_name <- function(short_name) {
  projects <- get_projects()
  ROW <- which(projects$short_name == short_name)
  OTHERS <- which(projects$short_name != short_name)
  if (!is_something(ROW)) message("Nothing to delete named: ", short_name) %>% return()
  projects <- projects[OTHERS, ]
  message("Deleted: ", short_name)
  save_projects_to_cache(projects)
  return(projects)
}
#' @noRd
internal_field_colnames <- c(
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
#' @noRd
form_colnames <- function(type) {
  if (missing(type)) type <- "default"
  if (type == "default") {
    c(
      "form_name",
      "form_label",
      "repeating",
      "repeating_via_events"
    ) %>% return()
  }
  if (type == "redcap") {
    c(
      "form_name",
      "form_label",
      "repeating",
      "repeating_via_events"
    ) %>% return()
  }
}
