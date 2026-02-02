#' @noRd
upload_form_to_redcap <- function(to_be_uploaded, project, batch_size = 500L) {
  REDCapR::redcap_write(
    ds_to_write = as_tibble(all_character_cols(to_be_uploaded)),
    batch_size = batch_size,
    interbatch_delay = 0.2,
    continue_on_error = FALSE,
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    overwrite_with_blanks = TRUE
  )
}
