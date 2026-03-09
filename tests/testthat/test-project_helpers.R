tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar("REDCAPSYNC_CACHE_OVERRIDE" = tempdir_file)
# labelled_to_raw_data_list (Internal)
test_that("labelled_to_raw_data_list and raw_to_labelled_data_list works!", {
  project <- mock_test_project()$.internal
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
  expect_all_true(project_converted$data$other$var_yesno %in% c("0", "1"))
  expect_no_error({
    project_again <- raw_to_labelled_data_list(project_converted)
  })
  expect_all_true(project_again$data$other$var_yesno %in% c("Yes", "No"))
})
# labelled_to_raw_form (Internal)
test_that("labelled_to_raw_form and raw_to_labelled_form works!", {
  project <- mock_test_project()$.internal
  project_summary <- generate_project_summary(project)
  merged <- all_character_cols(project_summary$merged)
  var_yesno_labelled <- merged$var_yesno
  values <- unique(merged$var_yesno)
  expect_vector(values, size = 2L)
  expect_contains(values, c("Yes", "No"))
  merged <- labelled_to_raw_form(merged, project)
  var_yesno_coded <- merged$var_yesno
  values <- unique(merged$var_yesno)
  expect_vector(values, size = 2L)
  expect_contains(values, c("1", "0"))
  var_yesno_labelled_check <- (var_yesno_labelled == "Yes") |>
    as.integer() |>
    as.character()
  expect_identical(var_yesno_labelled_check, var_yesno_coded)
  merged <- raw_to_labelled_form(merged, project)
  var_yesno_labelled_again <- merged$var_yesno
  values <- unique(merged$var_yesno)
  expect_vector(values, size = 2L)
  expect_contains(values, c("Yes", "No"))
  expect_identical(var_yesno_labelled, var_yesno_labelled_again)
  expect_in("Unknown", project$metadata$missing_codes$name)
  labelled <- merged |> select(record_id, var_branching)
  labelled$var_branching[3L] <- "Unknown"
  labelled$var_branching[5L] <- "Not applicable"
  raw <- labelled_to_raw_form(labelled, project)
  expect_identical("UNK", raw$var_branching[3L])
  expect_identical("NA", raw$var_branching[5L])
  labelled <- raw_to_labelled_form(raw, project)
  expect_identical("Unknown", labelled$var_branching[3L])
  expect_identical("Not applicable", labelled$var_branching[5L])
  mismatch <- merged |> select(record_id, var_branching)
  mismatch$var_branching[3L] <- "Random Thing"
  error_message <- "Mismatched REDCap"
  expect_error(labelled_to_raw_form(mismatch, project), error_message)
  raw_mismatch <- raw
  raw_mismatch$var_branching[3L] <- "Random Thing"
  expect_warning(raw_to_labelled_form(raw_mismatch, project), error_message)
})
# normalize_redcap (Internal)
test_that("normalize_redcap works with classic project", {
  project <- mock_test_project()$.internal
  call_list <- mock_test_calls()
  denormalized <- call_list$data
  result <- normalize_redcap(denormalized, project, labelled = TRUE)
  expect_type(result, type = "list")
  form_names <- names(result)
  expected_forms <- project$metadata$forms$form_name
  expect_all_true(expected_forms %in% form_names)
  expect_all_true(unlist(lapply(result, is.data.frame)))
  id_col <- project$metadata$id_col
  fields <- project$metadata$fields
  fields <- fields[which(fields$field_name != id_col), ]
  text_fields <- fields$field_name[which(fields$form_name == "text")]
  expect_true(id_col %in% colnames(result$text))
  expect_all_true(text_fields %in% colnames(result$text))
  rows_x <- which(fields$form_name == "other" &
                    (!fields$field_type %in% .field_types_not_in_data))
  other_fields <- fields$field_name[rows_x]
  expect_all_true(other_fields %in% colnames(result$other))
  expect_all_false(text_fields %in% colnames(result$other))
  expect_all_false(other_fields %in% colnames(result$text))
})
test_that("normalize_redcap works with longitudinal project", {
  project_name <- "TEST_REDCAPR_LONGITUDINAL"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  denormalized <- call_list$data
  result <- normalize_redcap(denormalized, project, labelled = TRUE)
  expect_true(project$metadata$is_longitudinal)
  expect_type(result, type = "list")
  form_names <- names(result)
  expected_forms <- project$metadata$forms$form_name
  expect_all_true(expected_forms %in% form_names)
  expect_all_true(unlist(lapply(result, is.data.frame)))
  id_col <- project$metadata$id_col
  fields <- project$metadata$fields
  expect_true(id_col %in% colnames(result$demographics))
  field_rows <- which(fields$form_name == "demographics" &
                        (!fields$field_type %in% .field_types_not_in_data))
  fields_to_check <- fields$field_name[field_rows]
  expect_all_true(fields_to_check %in% colnames(result$demographics))
  fields <- fields[which(fields$field_name != id_col), ]
  field_rows <-  which(fields$form_name == "demographics" &
                         (!fields$field_type %in% .field_types_not_in_data))
  fields_to_check <- fields$field_name[field_rows]
  expect_all_true(fields_to_check %in% colnames(result$demographics))
  expect_all_false(fields_to_check %in% colnames(result$other))
  expect_all_false(fields_to_check %in% colnames(result$text))
})
test_that("normalize_redcap works with repeating project", {
  project_name <- "TEST_REPEATING"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  denormalized <- call_list$data
  result <- normalize_redcap(denormalized, project, labelled = TRUE)
  expect_false(project$metadata$is_longitudinal)
  expect_type(result, type = "list")
  form_names <- names(result)
  expected_forms <- project$metadata$forms$form_name
  expect_all_true(expected_forms %in% form_names)
  expect_all_true(unlist(lapply(result, is.data.frame)))
  id_col <- project$metadata$id_col
  fields <- project$metadata$fields
  expect_true(id_col %in% colnames(result$form_1))
  field_rows <- which(fields$form_name == "form_1" &
                        (!fields$field_type %in% .field_types_not_in_data))
  fields_to_check <- fields$field_name[field_rows]
  expect_all_true(fields_to_check %in% colnames(result$form_1))
  extra_cols <- c("record_id",
                  "redcap_repeat_instrument",
                  "redcap_repeat_instance")
  field_rows <-  which(fields$form_name == "repeating" &
                         (!fields$field_type %in% .field_types_not_in_data))
  fields_to_check <- unique(append(extra_cols, fields$field_name[field_rows]))
  expect_all_true(fields_to_check %in% colnames(result$repeating))
  field_rows <-  which(fields$form_name == "repeating_2" &
                         (!fields$field_type %in% .field_types_not_in_data))
  fields_to_check <- unique(append(extra_cols, fields$field_name[field_rows]))
  expect_all_true(fields_to_check %in% colnames(result$repeating_2))
})
# raw_to_labelled_data_list (Internal)
test_that("raw_to_labelled_data_list works!", {
})
# raw_to_labelled_form (Internal)
test_that("raw_to_labelled_form works!", {
})
# construct_header_list (Internal)
test_that("construct_header_list works!", {
})
# extract_project_records (Internal)
test_that("extract_project_records works!", {
})
# extract_values_from_form_list (Internal)
test_that("extract_values_from_form_list works!", {
})
# field_names_metadata (Internal)
test_that("field_names_metadata works!", {
})
# field_names_to_form_names (Internal)
test_that("field_names_to_form_names works!", {
})
# filter_fields_from_form (Internal)
test_that("filter_fields_from_form works!", {
  project <- mock_test_project("TEST_REPEATING")$.internal
  form <- project$data$repeating_2 |> bind_rows(project$data$repeating)
  expect_error(filter_fields_from_form(form, project))
  expect_error(filter_fields_from_form(form, project))
  form <- project$data$form_1
  expect_no_error(filter_fields_from_form(form, project))
  fields <- filter_fields_from_form(form, project)
  expect_data_frame(fields, nrows = ncol(form))
})
# generate_choices_table (Internal)
test_that("generate_choices_table works!", {
})
# get_all_field_names (Internal)
test_that("get_all_field_names works!", {
  project <- mock_test_project()$.internal
  field_names <- get_all_field_names(project)
  expect_all_true(field_names %in% project$metadata$fields$field_name)
})
# get_identifier_fields (Internal)
test_that("get_identifier_fields works!", {
  # minimal metadata/fields to exercise deidentified / strict / super_strict
  field_names <- c("record_id", "email", "cell", "dob", "other", "notes", "int")
  valid_types <- c(NA, "email", "phone", "date_mdy", NA, NA, "integer")
  fields <- data.frame(
    field_name = field_names,
    identifier = c("y", NA, NA, "y", NA, NA, NA),
    field_type = c("text", "text", "text", "text", "text", "notes", "text"),
    text_validation_type_or_show_slider_number = valid_types,
    stringsAsFactors = FALSE
  )
  data_list <- list(metadata = list(fields = fields))
  data_list$metadata$form_key_cols <- "record_id"
  # deidentified: only explicit identifier flagged "y"
  out_deid <- get_identifier_fields(data_list, get_type = "deidentified")
  expect_identical(out_deid, c("record_id", "dob"))
  # deidentified_strict: includes fields with validation types
  # in strict list (email, phone)
  out_strict <- get_identifier_fields(data_list = data_list,
                                      get_type = "deidentified_strict")
  expect_identical(out_strict, c("record_id", "email", "cell", "dob"))
  # deidentified_super_strict: includes additional validation  (dates, etc.)
  out_super <- get_identifier_fields(data_list = data_list,
                                     get_type = "deidentified_super_strict")
  expect_identical(out_super, setdiff(field_names, "int"))
  # invert = TRUE should return the complement set
  out_inv <-get_identifier_fields(data_list = data_list,
                                  get_type = "deidentified",
                                  invert = TRUE)
  expect_identical(out_inv, c("email", "cell", "other", "notes", "int"))
})
# get_key_col_list (Internal)
test_that("get_key_col_list works!", {
})
# get_min_dates (Internal)
test_that("get_min_dates works!", {
  # construct minimal data_list with date fields across forms
  data_list <- list(
    data = list(
      form1 = data.frame(
        record_id = c("1", "2"),
        date1 = c("2020-01-01", "2020-01-05"),
        stringsAsFactors = FALSE
      ),
      form2 = data.frame(
        record_id = c("2", "3"),
        date2 = c("2020-02-01", "2020-01-15"),
        stringsAsFactors = FALSE
      )
    ),
    metadata = list(
      fields = data.frame(
        field_name = c("record_id", "date1", "date2"),
        field_type_R = c(NA, "date", "date"),
        stringsAsFactors = FALSE
      ),
      form_key_cols = list(record_id = "record_id")
    )
  )
  out <- get_min_dates(data_list)
  # basic structure checks
  expect_data_frame(out, nrows = 3L, ncols = 2L)
  expect_true(all(c("record_id", "date") %in% colnames(out)))
  # values: min date per record across forms
  one <- as.character(out$date[match("1", out$record_id)])
  two <- as.character(out$date[match("2", out$record_id)])
  three <- as.character(out$date[match("3", out$record_id)])
  expect_identical(one, "2020-01-01")
  expect_identical(two, "2020-01-05")
  expect_identical(three, "2020-01-15")
})
# get_project_url (Internal)
test_that("get_project_url works!", {
  project <- mock_test_project()$.internal
  project <- update_project_links(project)
  e <- new.env(parent = emptyenv())
  # get_project_url
  e$url <- NULL
  local_mocked_bindings(
    browseURL = function(url) {
      e$url <- url
    }
  )
  for (link_type in .link_types) {
    e$url <- NULL
    get_project_url(project, link_type = link_type, open_browser = TRUE)
    expect_identical(e$url, project$links[[paste0("redcap_", link_type)]])
    out <- get_project_url(project, link_type = link_type, open_browser = FALSE)
    expect_identical(out, project$links[[paste0("redcap_", link_type)]])
  }
})
# get_record_url (Internal)
test_that("get_record_url works!", {
  project <- mock_test_project()$.internal
  project$links$redcap_base <- "https://fakeredcap.com/"
  expected_link <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/DataEntry/record_home.php?pid=",
    project$redcap$project_id
  )
  e <- new.env(parent = emptyenv())
  # get_project_url
  e$url <- NULL
  # mockery::stub(get_record_url, "utils::browseURL", function(url) {
  #   e$url <- url
  # })
  local_mocked_bindings(
    browseURL = function(url) {
      e$url <- url
    }
  )
  expect_identical(get_record_url(project, open_browser = FALSE), expected_link)
  expect_null(e$url) # does not call url
  e$url <- NULL
  get_record_url(project, open_browser = TRUE)
  expect_identical(e$url, expected_link)
  e$url <- NULL
  expect_error(get_record_url(project, record = "59", open_browser = FALSE),
               regexp = "is not one of the records")
  expect_identical(
    get_record_url(project, record = "1", open_browser = FALSE),
    paste0(expected_link, "&id=1")
  )
  expect_identical(
    get_record_url(
      project,
      page = "text",
      open_browser = FALSE
    ),
    paste0(gsub("record_home", "index", expected_link), "&page=text")
  )
  expect_error(
    get_record_url(
      project,
      page = "text_2",
      open_browser = FALSE
    )
  )
})
