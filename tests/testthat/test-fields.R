tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
# add_project_field (Internal)
test_that("add_project_field and remove_project_fields works!", {
  project <- mock_test_project()
  expect_null(project$transformation$fields)
  expect_null(project$data$text$letter_b)
  expect_null(project$data$text$factor_sml)
  project$add_field(
    field_name = "letter_b",
    field_label = "Letter B?",
    form_name = "text",
    data_func = function(project) {
      project$data$text$var_text_letters == "b"
    }
  )
  expect_contains(project$transformation$fields$field_name, "letter_b")
  expect_function(project$transformation$field_functions$letter_b)
  project$add_field(
    field_name = "factor_sml",
    field_label = "Integer Size",
    form_name = "text",
    field_type_r = "factor",
    field_choices = c("Small", "Medium", "Large"),
    data_func = function(project) {
      nums <- as.integer(project$data$text$var_text_integer)
      final <- ifelse(nums <= 33,
                      "Small",
                      ifelse(nums <= 66,
                             "Medium",
                             "Large"))
      final # must be in same order as original
    }
  )
  expect_contains(project$transformation$fields$field_name, "factor_sml")
  expect_function(project$transformation$field_functions$factor_sml)
  in_original_int <- as.integer(project$data$text$var_text_integer)
  in_original_minus <- as.integer(project$data$text$var_text_integer) - 1L
  project$add_field(
    field_name = "var_text_integer",
    data_func = function(project) {
      as.character(as.integer(project$data$text$var_text_integer) - 1L)
    }
  )
  dataset <- project$generate_dataset("custom", exclude_identifiers = FALSE)
  expect_logical(dataset$data$merged$letter_b)
  expect_factor(dataset$data$merged$factor_sml)
  expect_equal(
    as.character(in_original_minus),
    as.character(dataset$data$merged$var_text_integer)
  )
  project$remove_added_fields()
  dataset <- project$generate_dataset("custom", exclude_identifiers = FALSE)
  expect_null(dataset$data$merged$letter_b)
  expect_null(project$data$merged$factor_sml)
  expect_equal(
    as.character(in_original_int),
    as.character(dataset$data$merged$var_text_integer)
  )
})
# clean_function (Internal)
test_that("clean_function works!", {
})
# choice_vector_string (Internal)
test_that("choice_vector_string works!", {
})
# remove_project_fields (Internal)
test_that("remove_project_fields works!", {
})
# remove_project_transformation (Internal)
test_that("remove_project_transformation works!", {
})
# add_project_transformation (Internal)
test_that("add_project_transformation and remove_project_transformation", {
  project <- mock_test_project("TEST_REPEATING")
  expect_null(project$transformation$custom)
  expect_contains(names(project$data), "repeating_2")
  ds <- project$generate_dataset()
  expect_contains(names(ds$data), "repeating_2")
  expect_false("var_systolic" %in% names(ds$data$merged))
  project$add_transformation(function(project) {
    forms <- project$metadata$forms
    forms$repeating[which(forms$form_name == "repeating_2")] <- FALSE
    project$metadata$forms <- forms
    project$metadata$repeating_forms_events <-
      project$metadata$repeating_forms_events[1L, ]
    rows <- which(project$data$repeating_2$redcap_repeat_instance == "1")
    project$data$repeating_2 <- project$data$repeating_2[rows, ]
    project
  })
  expect_function(project$transformation$custom)
  ds <- project$generate_dataset()
  expect_false("repeating_2" %in% names(ds$data))
  expect_contains(names(ds$data$merged), "var_systolic")
  project$remove_transformation()
  expect_null(project$transformation$custom)
})
# render_fields (Internal)
test_that("render_fields works!", {
})
# rerender_fields (Internal)
test_that("rerender_fields works!", {
})
# combine_project_fields (Internal)
test_that("combine_project_fields works!", {
})
# add_fields_to_data_list (Internal)
test_that("add_fields_to_data_list works!", {
})
