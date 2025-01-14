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
    file_path <- validate_dir(file_path)
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
#' @noRd
internal_blank_project_cols <- c(
  "short_name",
  "dir_path",
  "last_save",
  "last_metadata_update",
  "last_data_update",
  "version",
  "token_name",
  "project_id",
  "project_title",
  "id_col",
  "is_longitudinal",
  "has_repeating_forms_or_events",
  "has_multiple_arms",
  "R_object_size",
  "file_size",
  "n_records",
  "redcap_base",
  "redcap_home",
  "redcap_API_playground"
  # "test_dir"
  # "test_project",
  # "test_RC"
)
#' @noRd
blank_project <- function() {
  x <- matrix(data = character(0), ncol = length(internal_blank_project_cols)) %>% as.data.frame()
  colnames(x) <- internal_blank_project_cols
  return(x)
}
#' @noRd
save_projects_to_cache <- function(projects, silent = TRUE) {
  projects <- projects[order(projects$short_name), ]
  # projects$test_dir <- projects$test_dir %>% as.logical()
  # projects$test_project <- projects$test_project %>% as.logical()
  # projects$test_RC <- projects$test_RC %>% as.logical()
  saveRDS(projects, file = cache_path() %>% file.path("projects.rds"))
  if (!silent) {
    bullet_in_console(
      bullet_type = "v",
      text = paste0(pkg_name, " saved ", nrow(projects), " project locations to the cache...", paste0(projects$short_name, collapse = ", ")) # "   Token: ",projects$token_name,collapse = "\n"))
    )
    bullet_in_console(
      text = paste0("The cache is stored in directory on your computer. It can be found with `", pkg_name, "::cache_path()`, and cleared with `", pkg_name, "::cache_clear()`."),
      file = cache_path()
    )
  }
}
#' @noRd
extract_project_details <- function(project) {
  OUT <- data.frame(
    short_name = project$short_name,
    dir_path = project$dir_path %>% is.null() %>% ifelse(NA, sanitize_path(project$dir_path)),
    last_save = project$internals$last_data_dir_save %>% is.null() %>% ifelse(NA, project$internals$last_data_dir_save) %>% as.POSIXct(),
    last_metadata_update = project$internals$last_metadata_update,
    last_data_update = project$internals$last_data_update,
    version = project$redcap$version,
    token_name = project$redcap$token_name,
    project_id = project$redcap$project_id,
    project_title = project$redcap$project_title,
    id_col = project$redcap$id_col,
    is_longitudinal = project$redcap$is_longitudinal,
    has_repeating_forms_or_events = project$redcap$has_repeating_forms_or_events,
    has_multiple_arms = project$redcap$has_multiple_arms,
    n_records = ifelse(is.null(project$summary$all_records[[project$redcap$id_col]]), NA, project$summary$all_records %>% nrow()),
    R_object_size = NA,
    file_size = NA,
    redcap_base = project$links$redcap_base,
    redcap_home = project$links$redcap_home,
    redcap_API_playground = project$links$redcap_API_playground
  ) %>% all_character_cols()
  rownames(OUT) <- NULL
  return(OUT)
}
#' @noRd
add_project <- function(project, silent = TRUE) {
  projects <- get_projects()
  projects <- projects[which(projects$short_name != project$short_name), ]
  OUT <- extract_project_details(project = project)
  OUT$R_object_size <- size(project)
  OUT$file_size <- file.path(project$dir_path, "R_objects", paste0(project$short_name, "_REDCapSync.RData")) %>% file_size_mb()
  projects <- projects %>% dplyr::bind_rows(OUT)
  save_projects_to_cache(projects, silent = silent)
}
#' @noRd
delete_project_from_name <- function(short_name) {
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
