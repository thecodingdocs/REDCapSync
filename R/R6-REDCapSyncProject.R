#' @title Setup or Load REDCapSync Project
#' @description
#' Project class (REDCapSync)
#'
#' Main class for managing REDCap data, metadata, and sync operations.
#'
#' Users should construct objects using [setup_project()], not using
#' `REDCapSyncProject$new()` directly.
#'
#' @param short_name Character. The project short identifier.
#' @section Private Methods:
#'
#' \describe{
#'   \item{\code{load(short_name)}}{Load project internals.}
#' }
#' @return A `REDCapSyncProject` R6 object.
#'
#' @export
REDCapSyncProject <- R6::R6Class(
  "REDCapSyncProject",
  public = list(
    #' @description
    #' Creates a new instance of this REDCapSyncProject  [R6][R6::R6Class] class.
    #' @param description short_name Character project identifier.
    initialize = function(short_name) {
      if(missing(short_name)){
      }
      private$load(short_name)
    },
    #' @description Print project metadata
    info = function(){
      message("short_name: ",private$project$short_name)
      message("directory: ",private$project$dir_path)
    },
    #' @description  Add a new summary entry
    add_summary = function(){
      add_project_summary()
    },
    #' @description  Add a new summary entry
    generate_summary = function(short_name){
      private %>% generate_summary(short_name)
    },
    #' @description  Add a new summary entry
    add_field = function(){
      message("Added field!")
    },
    #' @description  Add a new summary entry
    sync = function(){
      private$project <- sync_project(private$project,summarize = FALSE)
    },
    #' @description  Add a new summary entry
    save = function(){
      save_project(private$project)
    },
    #' @description  returns internal list
    use = function(){
      invisible(private$project)
    }
  ),
  private = list(
    project = list(
      short_name = NULL,
      dir_path = NULL,
      redcap = list(
        token_name = NULL,
        project_id = NULL,
        project_title = NULL,
        version = NULL,
        project_info = NULL,
        log = NULL,
        users = NULL,
        current_user = NULL
      ),
      metadata = list(
        forms = NULL,
        fields = NULL,
        choices = NULL,
        id_col = NULL,
        form_key_cols = NULL,
        arms = NULL,
        events = NULL,
        event_mapping = NULL,
        missing_codes = NULL,
        raw_structure_cols = NULL,
        is_longitudinal = NULL,
        has_arms = NULL,
        has_multiple_arms = NULL,
        has_arms_that_matter = NULL,
        has_repeating_forms_or_events = NULL,
        has_repeating_forms = NULL,
        has_repeating_events = NULL
      ),
      data = NULL,
      quality_checks = NULL,
      transformation = list(
        forms = NULL,
        fields = NULL,
        field_functions = NULL,
        data_updates = NULL
      ),
      summary = list(),
      internals = list(
        last_test_connection_attempt = NULL,
        last_test_connection_timezone = NULL,
        last_test_connection_outcome = NULL,
        last_metadata_update = NULL,
        last_metadata_dir_save = NULL,
        last_full_update = NULL,
        last_data_update = NULL,
        last_data_dir_save = NULL,
        last_data_transformation = NULL,
        last_summary = NULL,
        last_quality_check = NULL,
        last_clean = NULL,
        last_directory_save = NULL,
        last_sync = NULL,
        labelled = NULL,
        timezone = NULL,
        get_files = NULL,
        get_file_repository = NULL,
        original_file_names = NULL,
        days_of_log = NULL,
        entire_log = NULL,
        data_extract_merged = NULL,
        project_type = "redcap",
        is_blank = TRUE,
        is_test = FALSE,
        ever_connected = FALSE,
        is_clean = FALSE,
        use_csv = FALSE
      ),
      links = list(
        redcap_uri = NULL,
        redcap_base = NULL,
        redcap_home = NULL,
        redcap_record_home = NULL,
        # redcap_record_subpage = NULL,
        redcap_records_dashboard = NULL,
        redcap_api = NULL,
        redcap_api_playground = NULL,
        redcap_setup = NULL,
        redcap_user_rights = NULL,
        redcap_logging = NULL,
        redcap_designer = NULL,
        redcap_codebook = NULL,
        redcap_dictionary = NULL,
        redcap_data_quality = NULL,
        redcap_identifiers = NULL,
        pkgdown = "https://thecodingdocs.github.io/REDCapSync/",
        github = "https://github.com/thecodingdocs/REDCapSync/",
        thecodingdocs = "https://www.thecodingdocs.com/"
      )
    ),
    load = function(short_name) {
      private$project <- load_project(short_name)
    }
  ),
  cloneable = FALSE
)
#' @title Setup or Load REDCapSync Project
#' @description
#' Setup or Load the `project` object for pipeline.
#'
#' @details
#' This function sets up the `project` object by storing the REDCap API token
#' and
#' other configurations required for interacting with the REDCap server.
#' It ensures that the token is valid and ready for use in subsequent API calls.
#' Neither function directly attempts communication with REDCap.
#'
#' `setup_project` is used the first time you initialize/link a REDCap project.
#' Mainly, it sets your unique `short_name` and your intended directory.
#' Unless you run \code{hard_reset = TRUE} the default will first try load_project.
#' dir_path is technically optional but without it the user cannot
#' save/load/update projects.
#'
#' `load_project` can be used with just the `short_name` parameter after you
#' have
#' already run `setup_project` in the past with an established directory.
#' `dir_path`
#' is optional for this function but can be used if you relocated the directory.
#' @inheritParams setup_project
#' @inheritParams REDCapR::redcap_read
#' @return REDCapSync `project` list object.
#' @seealso
#' \code{\link[REDCapSync]{get_projects}} for retrieving a list of projects from
#' the directory cache.
#' @examplesIf FALSE
#' # Initialize the project object with the REDCap API token and URL
#' project <- setup_project(
#'   short_name = "TEST",
#'   dir_path = "path/to/secure/file/storage",
#'   redcap_uri = "https://redcap.yourinstitution.edu/api/"
#' )
#' project <- load_project("TEST")
#' @export
setup_r6_test <- function(short_name,
                          dir_path,
                          redcap_uri,
                          token_name = paste0("REDCapSync_", short_name),
                          sync_frequency = "daily",
                          labelled = TRUE,
                          hard_reset = FALSE,
                          records = NULL,
                          fields = NULL,
                          forms = NULL,
                          events = NULL,
                          filter_logic = NULL,
                          get_type = "identified",
                          metadata_only = FALSE,
                          batch_size_download = 2000,
                          batch_size_upload = 500,
                          entire_log = FALSE,
                          days_of_log = 10,
                          get_files = FALSE,
                          get_file_repository = FALSE,
                          original_file_names = FALSE,
                          add_default_fields = FALSE,
                          add_default_transformation = FALSE,
                          add_default_summaries = TRUE,
                          use_csv = FALSE,
                          silent = FALSE){
  projects <- get_projects()
  REDCapSyncProject$new(short_name)
}
