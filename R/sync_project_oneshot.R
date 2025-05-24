sync_project_oneshot <- function(
    short_name,
    dir_path,
    redcap_base,
    redcap_uri,#show warning
    token,#show warning
    token_name = paste0("REDCapSync_", short_name),
    sync_frequency = "daily",
    get_type = "identified",
    metadata_only = FALSE,
    batch_size_download = 2000,
    batch_size_upload = 500,
    entire_log = FALSE,
    days_of_log = 10,
    get_files = FALSE,
    get_file_repository = FALSE,
    original_file_names = FALSE,
    merge_form_name = "merged",
    add_default_fields = FALSE,
    add_default_transformation = TRUE,
    add_default_summaries = TRUE,
    use_csv = FALSE,
    summarize = TRUE,
    save_to_dir = TRUE,
    hard_check = FALSE,
    hard_reset = FALSE,
    silent = FALSE
    ){
}
