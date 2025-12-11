# fields_to_choices ( Internal )
test_that("fields_to_choices works", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      fake_cache
    }
  )
  project <- mock_project()
  fields <- project$metadata$fields
  fields <- fields |> dplyr::bind_rows(
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
      )
    )
  )
  fields <- add_field_elements(fields)
  sum_of_choices <- 2 + 2 + 2 + 7 + 6 * (2) + 3
  choices <- fields_to_choices(fields)
  checkmate::expect_data_frame(choices, nrows = sum_of_choices)
  expect_false(any(is.na(choices$code)))
  expect_false(any(is.na(choices$name)))
  expect_false(any(is.na(choices$field_name)))
  expect_false(any(is.na(choices$form_name)))
  should_be_unique <- paste0(choices$field_name, "_", choices$code)
  #check warning for same name different code here?
  expect_false(anyDuplicated(should_be_unique) > 0)
})
# add_labels_to_checkbox ( Internal )
test_that("add_labels_to_checkbox works!", {
})
# annotate_fields ( Internal )
test_that("annotate_fields works!", {
})
# annotate_forms ( Internal )
test_that("annotate_forms works!", {
})
# annotate_choices ( Internal )
test_that("annotate_choices works!", {
})
# annotate_records ( Internal )
test_that("annotate_records works!", {
})
# clean_form ( Internal )
test_that("clean_form works!", {
})
# clean_column_for_table ( Internal )
test_that("clean_column_for_table works!", {
})
# add_project_summary ( Exported )
test_that("add_project_summary works!", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      fake_cache
    }
  )
  project <- mock_project()
  summary_name <- "MY_SUMMARY_TEST"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
    transformation_type = "default",
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
  s <- project$summary[[summary_name]]
  expect_equal(s$summary_name, summary_name)
  expect_equal(s$transformation_type, "default")
  expect_equal(s$include_metadata, TRUE)
  expect_equal(s$include_records, FALSE)
  expect_equal(s$include_users, TRUE)
  expect_equal(s$with_links, FALSE)
  expect_equal(s$separate, FALSE)
  expect_true(is.character(s$file_name))
})
# save_summary ( Internal )
test_that("save_summary works", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      fake_cache
    }
  )
  project <- TEST_CLASSIC
  project$dir_path <- test_dir
  dir.create(file.path(test_dir, "output"), recursive = TRUE)
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
    project <- save_summary(project, summary_name)
  })
  # file created
  file_path <- project$summary[[summary_name]]$file_path
  expect_true(file.exists(file_path))
  # summary metadata updated
  expect_true(!is.null(project$summary[[summary_name]]$n_records))
  expect_true(!is.null(project$summary[[summary_name]]$last_save_time))
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
})
# generate_project_summary ( Exported )
test_that("generate_project_summary works!", {
  project <- TEST_CLASSIC
  project_summary <- project |>
    generate_project_summary(summary_name = "REDCapSync")
  expect_true(is_df_list(project_summary))
  project_summary <- project |>
    generate_project_summary(include_metadata = TRUE)
  expect_contains(names(project_summary), "forms")
  expect_contains(names(project_summary), "fields")
  expect_contains(names(project_summary), "choices")
  project_summary <- project |>
    generate_project_summary(include_metadata = FALSE)
  expect_false("forms" %in% names(project_summary))
  expect_false("fields" %in% names(project_summary))
  expect_false("choices" %in% names(project_summary))
  project_summary <- project |>
    generate_project_summary(
      summary_name = "REDCapSync", exclude_identifiers = FALSE)
  fields <- project$metadata$fields
  fields$field_name[which(fields$identifier == "y")]
  colnames(project_summary$merged)
})
# merge_non_repeating ( Internal )
test_that("merge_non_repeating works!", {
  project <- TEST_CLASSIC
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
  # expect_identical(text_field_names,text_field_names2)
  # expect_identical(other_field_names,other_field_names2)
  merge_form_name <- "merged_form"
  merged <- merge_non_repeating(
    data_list = project,
    merge_form_name = merge_form_name,
    merge_to_rep = TRUE
  ) #NA for classic)
  expect_equal(merge_form_name, names(merged$data))
  expect_equal(nrow(merged$data[[merge_form_name]]), nrow(project$data$text))
  expect_equal(nrow(merged$data[[merge_form_name]]), nrow(project$data$other))
  expected_col_names <-  c(id_col, text_field_names, other_field_names)
  expected_col_length <- expected_col_names |> length_unique()
  expect_equal(ncol(merged$data[[merge_form_name]]), expected_col_length)
  expect_equal(names(merged$data[[merge_form_name]]), expected_col_names)
})
# summarize_project ( Internal )
test_that("summarize_project works", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      fake_cache
    }
  )
  project <- TEST_CLASSIC
  project$dir_path <- set_dir(test_dir)
  dir.create(
    path = file.path(project$dir_path, "REDCap", project$project_name),
    showWarnings = FALSE
  )
  expect_true(file.exists(project$dir_path))
  # ensure default summaries present
  project <- clear_project_summaries(project)
  project <- add_default_summaries(
    project = project,
    exclude_identifiers = TRUE,
    exclude_free_text = TRUE,
    date_handling = "none"
  )
  # run summarize_project
  expect_no_error({
    project_saved <- project |> summarize_project(hard_reset = TRUE)
  })
  # internals updated
  expect_true(!is.null(project_saved$internals$last_summary))
  expect_s3_class(project_saved$internals$last_summary, "POSIXt")
  # main summary file exists (REDCapSync should be written as a single file)
  expect_true("REDCapSync" %in% names(project_saved$summary))
  expect_true(file.exists(project_saved$summary$REDCapSync$file_path))
})
# clear_project_summaries ( Exported )
test_that("clear_project_summaries works!", {
  project <- TEST_CLASSIC
  summaries <- c("REDCapSync", "REDCapSync_raw")
  expect_all_true(summaries %in% names(project$summary))
  expect_all_true(summaries %in% names(project$summary$all_records))
  project <- clear_project_summaries(project)
  expect_all_false(summaries %in% names(project$summary))
  expect_all_false(summaries %in% names(project$summary$all_records))
})
# extract_values_from_form_list ( Internal )
test_that("extract_values_from_form_list works!", {
})
# extract_project_records ( Internal )
test_that("extract_project_records works!", {
})
# get_log ( Internal )
test_that("get_log works!", {
  project <- TEST_CLASSIC
  records <- as.character(1:50) |> sample(10)
  log <- project$redcap$log
  all_record_ids <- log$record |> unique()
  expect_length(all_record_ids, 50)
  log_subset <- get_log(project, records)
  subset_record_ids <- log_subset$record |> unique()
  expect_length(subset_record_ids, 10)
  expect_contains(subset_record_ids, records)
})
# annotate_users ( Internal )
test_that("annotate_users works!", {
})
# get_summary_records ( Internal )
test_that("get_summary_records works!", {
  project <- TEST_CLASSIC
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
  expect_equal(record_ids_yes, get_sum_records_yes)
  expect_equal(record_ids_no, get_sum_records_no)
})
# summary_records_due ( Internal )
test_that("summary_records_due works!", {
})
# check_summaries ( Internal )
test_that("check_summaries works!", {
})
# add_default_summaries ( Internal )
test_that("add_default_summaries works", {
  project <- TEST_CLASSIC |> clear_project_summaries()
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
  expect_equal(raw$transformation_type, "none")
  expect_false(raw$include_records)
  expect_true(raw$separate)
  # check expected settings for main summary
  expect_equal(main$transformation_type, "default")
  expect_true(main$include_records)
  expect_false(main$separate)
})
# labelled_to_raw_form ( Exported )
test_that("labelled_to_raw_form and raw_to_labelled_form works!", {
  project <- TEST_CLASSIC
  project_summary <- project |> generate_project_summary()
  merged <- project_summary$merged |> all_character_cols()
  var_yesno_labelled <- merged$var_yesno
  values <- merged$var_yesno |> unique()
  expect_vector(values, size = 2)
  expect_contains(values, c("Yes", "No"))
  merged <- merged |> labelled_to_raw_form(project)
  var_yesno_coded <- merged$var_yesno
  values <- merged$var_yesno |> unique()
  expect_vector(values, size = 2)
  expect_contains(values, c("1", "0"))
  var_yesno_labelled_check <- (var_yesno_labelled == "Yes") |>
    as.integer() |>
    as.character()
  expect_identical(var_yesno_labelled_check, var_yesno_coded)
  merged <- merged |> raw_to_labelled_form(project)
  var_yesno_labelled_again <- merged$var_yesno
  values <- merged$var_yesno |> unique()
  expect_vector(values, size = 2)
  expect_contains(values, c("Yes", "No"))
  expect_identical(var_yesno_labelled, var_yesno_labelled_again)
})
# labelled_to_raw_data_list ( Internal )
test_that("labelled_to_raw_data_listand raw_to_labelled_data_list works!", {
  project <- TEST_CLASSIC
  # ensure project is marked as labelled and has labelled values
  expect_true(project$internals$labelled)
  # sanity check: labelled value present in example form
  expect_all_true(project$data$other$var_yesno %in% c("Yes", "No"))
  # convert and capture returned project
  expect_no_error({
    project_converted <- labelled_to_raw_data_list(project)
  })
  # internals updated
  expect_false(project_converted$internals$labelled)
  # values converted from labelled ("Yes"/"No") to raw codes ("1"/"0")
  expect_all_true(
    project_converted$data$other$var_yesno %in% c("0", "1"))
  expect_no_error({
    project_again <- raw_to_labelled_data_list(project_converted)
  })
  expect_all_true(
    project_again$data$other$var_yesno %in% c("Yes", "No"))
})
# get_all_field_names ( Internal )
test_that("get_all_field_names works!", {
  project <- TEST_CLASSIC
  field_names <- get_all_field_names(project)
  expect_all_true(field_names %in% project$metadata$fields$field_name)
})
# get_all_field_names ( Internal )
test_that("get_identifier_fields works", {
  # minimal metadata/fields to exercise deidentified / strict / super_strict
  fields <- data.frame(
    field_name = c("record_id", "email", "phone", "dob", "other"),
    identifier = c("y", NA, NA, NA, NA),
    text_validation_type_or_show_slider_number =
      c(NA, "email", "phone", "date_mdy", NA),
    stringsAsFactors = FALSE
  )
  data_list <- list(metadata = list(fields = fields))
  # deidentified: only explicit identifier flagged "y"
  out_deid <- get_identifier_fields(data_list, get_type = "deidentified")
  expect_true(length(out_deid) == 1)
  expect_true("record_id" %in% out_deid)
  # deidentified_strict: includes fields with validation types
  # in strict list (email, phone)
  out_strict <-
    get_identifier_fields(data_list, get_type = "deidentified_strict")
  expect_true(setequal(out_strict, c("record_id", "email", "phone")))
  # deidentified_super_strict: includes additional validation  (dates, etc.)
  out_super <-
    get_identifier_fields(data_list, get_type = "deidentified_super_strict")
  expect_true(setequal(out_super, c("record_id", "email", "phone", "dob")))
  # invert = TRUE should return the complement set
  out_inv <-
    get_identifier_fields(data_list, get_type = "deidentified", invert = TRUE)
  expect_true(setequal(out_inv, setdiff(fields$field_name, "record_id")))
})
# field_names_to_form_names ( Internal )
test_that("field_names_to_form_names works!", {
})
# construct_header_list ( Internal )
test_that("construct_header_list works!", {
})
# field_names_metadata ( Internal )
test_that("field_names_metadata works!", {
})
# filter_fields_from_form ( Internal )
test_that("filter_fields_from_form works!", {
})
