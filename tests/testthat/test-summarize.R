tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar("REDCAPSYNC_CACHE_OVERRIDE" = tempdir_file)
# add_default_summaries (Internal)
test_that("add_default_summaries works!", {
  project <- mock_test_project()$.internal
  project <- clear_project_summaries(project)
  summaries <- c("REDCapSync", "REDCapSync_raw")
  expect_all_false(summaries %in% names(project$summary))
  expect_all_false(summaries %in% names(project$summary$all_records))
  expect_no_error({
    project <- add_default_summaries(
      project = project,
      exclude_identifiers = TRUE,
      exclude_free_text = TRUE,
      date_handling = "none"
    )
  })
  expect_all_true(summaries %in% names(project$summary))
  expect_all_true(summaries %in% names(project$summary$all_records))
  raw <- project$summary$REDCapSync_raw
  main <- project$summary$REDCapSync
  # check expected settings for raw summary
  expect_identical(raw$transformation_type, "none")
  expect_false(raw$include_records)
  expect_true(raw$separate)
  # check expected settings for main summary
  expect_identical(main$transformation_type, "default")
  expect_true(main$include_records)
  expect_false(main$separate)
})
# add_labels_to_checkbox (Internal)
test_that("add_labels_to_checkbox works!", {
})
# add_project_summary (Internal)
test_that("add_project_summary works!", {
  project <- mock_test_project()$.internal
  summary_name <- "MY_SUMMARY_TEST"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
    transformation_type = "default",
    drop_blanks = FALSE,
    drop_missing_codes = TRUE,
    include_metadata = TRUE,
    include_records = FALSE,
    include_users = TRUE,
    include_log = FALSE,
    annotate_from_log = FALSE,
    with_links = FALSE,
    separate = FALSE,
    use_csv = FALSE
  )
  expect_true(summary_name %in% names(project$summary))
  summary <- project$summary[[summary_name]]
  expect_identical(summary$summary_name, summary_name)
  expect_identical(summary$transformation_type, "default")
  expect_true(summary$include_metadata)
  expect_false(summary$include_records)
  expect_true(summary$include_users)
  expect_false(summary$with_links)
  expect_false(summary$separate)
  expect_type(summary$file_name, type = "character")
})
# annotate_choices (Internal)
test_that("annotate_choices works!", {
})
# annotate_fields (Internal)
test_that("annotate_fields works!", {
})
# annotate_forms (Internal)
test_that("annotate_forms works!", {
})
# annotate_records (Internal)
test_that("annotate_records works!", {
})
# annotate_users (Internal)
test_that("annotate_users works!", {
})
# check_summaries (Internal)
test_that("check_summaries works!", {
})
# clean_column_for_table (Internal)
test_that("clean_column_for_table works!", {
})
# clean_data_list (Internal)
test_that("clean_data_list works!", {
})
# clean_form (Internal)
test_that("clean_form works!", {
  project <- mock_test_project()$.internal
  form <- project$data$text
  project <- metadata_add_default_cols(project)
  fields <- project$metadata$fields
  form <- clean_form(form = form, fields = fields)
  expect_identical(attr(form$var_text_only, "label"), "Text Only")
  expect_identical(attr(form$var_birth_date, "label"), "Birth Date")
  row_to_change <- which(fields$field_name == "var_multi_radio")
  new_var <- "1, 1 | 2, 2 | 3, 3 | 4, 4 | 5, 5 | 6, 5"
  fields$select_choices_or_calculations[row_to_change] <- new_var
  form <- project$data$other
  expect_warning(clean_form(form = form, fields = fields), "dupplicate names")
})
# clear_project_summaries (Internal)
test_that("clear_project_summaries works!", {
  project <- mock_test_project()$.internal
  summaries <- c("REDCapSync", "REDCapSync_raw")
  expect_all_true(summaries %in% names(project$summary))
  expect_all_true(summaries %in% names(project$summary$all_records))
  project <- clear_project_summaries(project)
  expect_all_false(summaries %in% names(project$summary))
  expect_all_false(summaries %in% names(project$summary$all_records))
})
# data_list_to_save (Internal)
test_that("data_list_to_save works!", {
})
# deidentify_data_list (Internal)
test_that("deidentify_data_list works!", {
  project <- mock_test_project()$.internal
  data_list <- merge_non_repeating(TEST_CLASSIC, "merged")
  data_list$metadata$fields$field_type_R <- NA
  data_list$metadata$fields$in_original_redcap <- NA
  id_cols <- data_list$metadata$form_key_cols |>
    unlist() |>
    unique()
  fields <- data_list$metadata$fields
  is_identifier <- fields$identifier == "y"
  fields$validation_type <- fields$text_validation_type_or_show_slider_number
  is_likely_identifier <- fields$validation_type %in% .redcap_maybe_ids_strict
  identifier_rows <- which(is_identifier | is_likely_identifier)
  initial_identifiers <- fields$field_name[identifier_rows]
  is_notes <- fields$field_type == "notes"
  is_text <- fields$field_type == "text"
  has_validation <- is.na(fields$text_validation_type_or_show_slider_number)
  not_id <- !fields$field_name %in% id_cols
  is_free_text <- is_notes | (is_text & has_validation) & not_id
  free_text_fields <- fields$field_name[which(is_free_text)]
  expect_all_true(initial_identifiers %in% colnames(data_list$data$merged))
  no_ids <- deidentify_data_list(data_list = data_list,
                                 exclude_identifiers = TRUE,
                                 exclude_free_text = FALSE)
  expect_all_false(initial_identifiers %in% colnames(no_ids$data$merged))
  expect_all_true(free_text_fields %in% colnames(data_list$data$merged))
  no_free_text <- deidentify_data_list(data_list = data_list,
                                       exclude_identifiers = FALSE,
                                       exclude_free_text = TRUE)
  expect_all_false(initial_identifiers %in% colnames(no_free_text$data$merged))
  keep_rows <-
    which(!data_list$metadata$fields$field_name %in% initial_identifiers)
  data_list$metadata$fields <- data_list$metadata$fields[keep_rows, ]
  keep_cols <- which(!colnames(data_list$data$merged) %in% initial_identifiers)
  data_list$data$merged <- data_list$data$merged[, keep_cols]
  expect_warning(
    deidentify_data_list(
      data_list = data_list,
      exclude_identifiers = TRUE,
      exclude_free_text = FALSE
    ),
    "You have no identifiers marked"
  )
})
test_that("deidentify_data_list works2", {
  project <- mock_test_project()$.internal
  data_list <- merge_non_repeating(TEST_CLASSIC, "merged")
  data_list <- metadata_add_default_cols(data_list)
  fields <- data_list$metadata$fields
  merged <- data_list$data$merged
  expected_cols <- c("var_birth_date",
                     "var_text_date_dmy",
                     "var_text_date_mdy",
                     "var_text_date_ymd")
  expect_all_true(expected_cols %in% colnames(merged))
  expect_error(deidentify_data_list(data_list = data_list, date_handling = "1"))
  # 'none'
  merged_none <- deidentify_data_list(
    data_list = data_list,
    date_handling = "none"
  )$merged
  expect_all_true(expected_cols %in% colnames(merged_none))
  # 'exclude_dates'
  excluded_dates <- deidentify_data_list(data_list = data_list,
                                         date_handling = "exclude_dates")$merged
  expect_all_false(expected_cols %in% colnames(excluded_dates))
  # 'random_shift_by_record'
  random_shift <- deidentify_data_list(data_list = data_list,
                                       date_handling = "random_shift_by_record")
  random_shift <- random_shift$merged
  expect_all_true(expected_cols %in% colnames(random_shift))
  expect_all_false(random_shift$var_text_date_dmy == merged$var_text_date_dmy)
  time_check1 <- as.Date(merged$var_text_date_dmy) -
    as.Date(merged$var_birth_date)
  time_check2 <- as.Date(random_shift$var_text_date_dmy) -
    as.Date(random_shift$var_birth_date)
  expect_all_true(time_check1 == time_check2) #math is same
  as.Date(merged$var_birth_date) - as.Date(random_shift$var_birth_date)
  # 'random_shift_by_project'
  # merged_random_shift_by_project <- deidentify_data_list(
  #   data_list = data_list,date_handling = "random_shift_by_project")
  # # 'zero_by_record'
  # merged_zero_by_record <- deidentify_data_list(
  #   data_list = data_list,date_handling = "zero_by_record")
  # # 'zero_by_project'
  # merged_zero_by_project <- deidentify_data_list(
  #   data_list = data_list,date_handling = "zero_by_project")
})
# field_types_to_R (Internal)
test_that("field_types_to_R works!", {
})
# fields_to_choices (Internal)
test_that("fields_to_choices works!", {
  project <- mock_test_project()$.internal
  fields <- project$metadata$fields
  fields <- fields[0L, ] |> dplyr::bind_rows(
    data.frame(
      field_name = c("sex", "yes_no1", "yes_no2", "race", "location"),
      form_name = "form_one",
      field_type = c("radio", "yesno", "yesno", "dropdown", "checkbox"),
      select_choices_or_calculations = c(
        "0, Female | 1, Male",
        # 2
        "0, No | 1, Yes",
        # 2
        "0,No | 1,Yes",
        # 2
        paste0(
          "0, American Indian/Alaska Native",
          " | ",
          "1, Asian",
          " | ",
          "2, Native Hawaiian or Other Pacific Islander",
          " | ",
          "3, Black or African American",
          " | ",
          "4, White",
          " | ",
          "5, More Than One Race",
          " | ",
          "6, Unknown / Not Reported"
        ),
        # 7
        "1, Lung | 2, Liver | 3, Bone | 4, Lymph node | 5, Other | 6, Brain" #6
      ),
      stringsAsFactors = FALSE
    )
  )
  fields <- add_field_elements(fields)
  sum_of_choices <- 2L + 2L + 2L + 7L + 6L * (2L) + 3L
  choices <- fields_to_choices(fields)
  expect_data_frame(choices, nrows = sum_of_choices)
  expect_false(anyNA(choices$code))
  expect_false(anyNA(choices$name))
  expect_false(anyNA(choices$field_name))
  expect_false(anyNA(choices$form_name))
  should_be_unique <- paste0(choices$field_name, "_", choices$code)
  #check warning for same name different code here?
  expect_false(anyDuplicated(should_be_unique) > 0L)
})
# filter_data_list (Internal)
test_that("filter_data_list works!", {
})
# flatten_redcap (Internal)
test_that("flatten_redcap works!", {
})
# generate_project_summary (Internal)
test_that("generate_project_summary works!", {
  project <- mock_test_project()$.internal
  expect_error(generate_project_summary(project = project,
                                        summary_name = "non-existing"),
               regexp = "not included in the current project summaries")
  project_summary <- generate_project_summary(project = project,
                                              summary_name = "REDCapSync")
  expect_true(is_df_list(project_summary))
  project_summary <- generate_project_summary(project = project,
                                              include_metadata = TRUE,
                                              include_log = TRUE)
  expect_contains(names(project_summary), "forms")
  expect_contains(names(project_summary), "fields")
  expect_contains(names(project_summary), "choices")
  project_summary <- generate_project_summary(project = project,
                                              include_metadata = FALSE)
  expect_false("forms" %in% names(project_summary))
  expect_false("fields" %in% names(project_summary))
  expect_false("choices" %in% names(project_summary))
  project_summary <- generate_project_summary(project = project,
                                              summary_name = "REDCapSync",
                                              exclude_identifiers = FALSE)
  fields <- project$metadata$fields
  fields$field_name[which(fields$identifier == "y")]
  colnames(project_summary$merged)
  should_be_missing <- project_summary$merged$var_text_datetime_ymd_hm[1]
  expect_identical(should_be_missing, "Unknown")
  project_summary <- generate_project_summary(project = project,
                                              drop_missing_codes = TRUE)
  should_not_be_missing <- project_summary$merged$var_text_datetime_ymd_hm[1]
  expect_scalar_na(should_not_be_missing)
})
# get_log (Internal)
test_that("get_log works!", {
  project <- mock_test_project()$.internal
  log <- project$redcap$log
  records <- unique(log$record) |> sample(1L)
  log_subset <- get_log(project, records)
  subset_record_id <- unique(log_subset$record)
  expect_in(subset_record_id, records)
})
# get_summary_records (Internal)
test_that("get_summary_records works!", {
  project <- mock_test_project()$.internal
  project <- project |>
    add_project_summary(
      summary_name = "test_branching_yes",
      filter_field = "var_branching",
      filter_choices = "Yes"
    ) |>
    add_project_summary(
      summary_name = "test_branching_no",
      filter_field = "var_branching",
      filter_choices = "No"
    )
  other <- project$data$other
  expect_true("test_branching_yes" %in% names(project$summary))
  expect_true("test_branching_no" %in% names(project$summary))
  record_ids_yes <- other$record_id[which(other$var_branching == "Yes")] |>
    sort()
  record_ids_no <- other$record_id[which(other$var_branching == "No")] |>
    sort()
  get_sum_records_yes <- project |>
    get_summary_records("test_branching_yes") |>
    sort()
  get_sum_records_no <- project |>
    get_summary_records("test_branching_no") |>
    sort()
  expect_identical(record_ids_yes, get_sum_records_yes)
  expect_identical(record_ids_no, get_sum_records_no)
})
# merge_non_repeating (Internal)
test_that("merge_non_repeating works!", {
  project <- mock_test_project()$.internal
  expect_contains(names(project$data), project$metadata$forms$form_name)
  id_col <- project$metadata$id_col
  text_field_names <- colnames(project$data$text) |> setdiff(id_col)
  other_field_names <- colnames(project$data$other) |> setdiff(id_col)
  text_field_names2 <- project$metadata$fields |>
    get_match(
      match_field = "form_name",
      match_text = "text",
      return_field = "field_name"
    ) |>
    setdiff(id_col)
  other_field_names2 <- project$metadata$fields |>
    get_match(
      match_field = "form_name",
      match_text = "other",
      return_field = "field_name"
    ) |>
    setdiff(id_col)
  merge_form_name <- "merged_form"
  merged <- merge_non_repeating(
    data_list = project,
    merge_form_name = merge_form_name,
    merge_to_rep = TRUE
  ) #NA for classic)
  expect_identical(merge_form_name, names(merged$data))
  a <- nrow(merged$data[[merge_form_name]])
  b <- nrow(project$data$text)
  c <- nrow(project$data$other)
  expect_identical(a, b)
  expect_identical(a, c)
  expected_col_names <-  c(id_col, text_field_names, other_field_names)
  expected_col_length <- length_unique(expected_col_names)
  expect_identical(ncol(merged$data[[merge_form_name]]), expected_col_length)
  expect_named(merged$data[[merge_form_name]], expected_col_names)
})
# metadata_add_default_cols (Internal)
test_that("metadata_add_default_cols works!", {
})
# save_project_summary (Internal)
test_that("save_project_summary works!", {
  project <- mock_test_project()$.internal
  summary_name <- "SAVE_SUMMARY_TEST"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
    transformation_type = "default",
    include_metadata = TRUE,
    include_records = TRUE,
    include_users = TRUE,
    include_log = FALSE,
    annotate_from_log = FALSE,
    with_links = FALSE,
    separate = FALSE,
    use_csv = FALSE
  )
  # save and capture returned project
  expect_no_error({
    project <- save_project_summary(project, summary_name)
  })
  # file created
  file_path <- project$summary[[summary_name]]$file_path
  expect_file_exists(file_path)
  # summary metadata updated
  expect_false(is.null(project$summary[[summary_name]]$n_records))
  expect_false(is.null(project$summary[[summary_name]]$last_save_time))
  what_was_saved <- excel_to_list(file_path)
  expect_all_true(
    c(
      "merged",
      "forms",
      "fields",
      "choices",
      "missing_codes",
      "users",
      "records",
      "summary_details"
    ) %in% names(what_was_saved)
  )
  # csv
  summary_name <- "SAVE_SUMMARY_TEST_CSV"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
    transformation_type = "default",
    include_metadata = TRUE,
    include_records = TRUE,
    include_users = TRUE,
    include_log = FALSE,
    annotate_from_log = FALSE,
    with_links = FALSE,
    separate = FALSE,
    use_csv = TRUE
  )
  # save and capture returned project
  expect_no_error({
    project <- save_project_summary(project, summary_name)
  })
  # file created
  file_path <- file.path(project$dir_path,
                         "output",
                         paste0("TEST_CLASSIC_SAVE_SUMMARY_TEST_CSV",
                                c("_merged.csv",
                                  "_forms.csv",
                                  "_summary_details.csv")))
  expect_file_exists(file_path)
})
# summarize_project (Internal)
test_that("summarize_project works!", {
  project <- mock_test_project()$.internal
  expect_directory_exists(project$dir_path)
  # ensure default summaries present
  project <- clear_project_summaries(project)
  project <- add_default_summaries(
    project = project,
    exclude_identifiers = TRUE,
    exclude_free_text = TRUE,
    date_handling = "none"
  )
  expect_no_error({
    project_saved <- summarize_project(project, hard_reset = TRUE)
  })
  expect_false(is.null(project_saved$internals$last_summary))
  expect_s3_class(project_saved$internals$last_summary, "POSIXt")
  expect_true("REDCapSync" %in% names(project_saved$summary))
  expect_file_exists(project_saved$summary$REDCapSync$file_path)
})
# summary_records_due (Internal)
test_that("summary_records_due works!", {
})
