mock_project <- function() {
  project_name <- "TEST_PROJECT"
  redcap_uri <- "https://redcap.fake.edu/api/"
  project <- setup_project(
    project_name = project_name,
    redcap_uri = redcap_uri
  )$.internal() # change to R6 later
  project$internals$ever_connected <- TRUE
  fake_time <- now_time()
  project$internals$last_directory_save <- fake_time
  project$internals$last_metadata_update <- fake_time
  project$internals$last_data_update <- fake_time
  project$internals$last_sync <- fake_time
  project$internals$timezone <- Sys.timezone()
  project$internals$is_test <- TRUE
  project$redcap$version <- "12.1.1"
  project$redcap$token_name <- "REDCapSync_TEST_PROJECT"
  project$redcap$project_id <- "01234"
  project$redcap$project_title <- "A Fake Project"
  project$redcap$id_col <- "record_id"
  project$redcap$is_longitudinal <- FALSE
  project$redcap$has_repeating_forms_or_events <- FALSE
  project$redcap$has_multiple_arms <- FALSE
  # metadata -------
  project$metadata$fields <- data.frame(
    field_name = character(0),
    form_name = character(0),
    section_header = character(0),
    field_type = character(0),
    field_label = character(0),
    select_choices_or_calculations = character(0),
    field_note = character(0),
    text_validation_type_or_show_slider_number = character(0),
    text_validation_min = character(0),
    text_validation_max = character(0),
    identifier = character(0),
    branching_logic = character(0),
    required_field = character(0),
    custom_alignment = character(0),
    question_number = character(0),
    matrix_group_name = character(0),
    matrix_ranking = character(0),
    field_annotation = character(0)
  )
  project$data$form_example <- project$data$form_example %>%
    dplyr::bind_rows(
      data.frame(
        field_name = "var_yesno",
        form_name = "form_example",
        field_type = "yesno",
        field_label = "Variable Yes/No",
        select_choices_or_calculations = "0, No | 1, Yes",
        field_note = NA,
        text_validation_type_or_show_slider_number = NA,
        text_validation_min = NA,
        text_validation_max = NA,
        identifier = NA,
        branching_logic = NA,
        required_field = NA,
        custom_alignment = NA,
        question_number = NA,
        matrix_group_name = NA,
        matrix_ranking = NA,
        field_annotation = NA
      )
    )
  project$metadata$forms <- data.frame(
    form_name = character(0),
    form_label = character(0),
    repeating = character(0)
  )
  #data -------
  project$data$form_example <- data.frame(
    record_id = character(0),
    var_id = character(0),
    var_free_text = character(0),
    var_text_date = character(0),
    var_text_int = character(0),
    var_yesno = character(0)
  )
  project$data$form_example <- project$data$form_example %>%
    dplyr::bind_rows(
      data.frame(
        record_id = "1",
        var_id = "123-12-1993",
        var_free_text = "free falling",
        var_text_date = "2012-12-12",
        var_text_int = "2",
        var_yesno = "Yes"
      )
    )
  # form_example
  return(project)
}
