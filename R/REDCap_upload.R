#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your
#' own risk.
#' Remember all changes are saved in the redcap log if there's an issue. Missing
#' rows and columns are fine!
#' @inheritParams save_project
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean
#' data pass your data to `labelled_to_raw_form(form,project)` first.
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
upload_form_to_REDCap <- function(to_be_uploaded, project, batch_size = 500) {
  REDCapR::redcap_write(
    ds_to_write = to_be_uploaded %>% all_character_cols(),
    batch_size = batch_size,
    interbatch_delay = 0.2,
    continue_on_error = FALSE,
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    overwrite_with_blanks = TRUE
  )
}
