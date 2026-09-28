tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
# add_project_field (Internal)
test_that("add_project_field works!", {
  project <- mock_test_project()
  expect_null(project$data$text$letter_b)
  project$add_field(
    field_name = "letter_b",
    field_label = "Letter B?",
    form_name = "text",
    data_func = function(project) {
      project$data$text$var_text_letters == "b"
    }
  )
  project$add_field(
    field_name = "factor_sml",
    field_label = "Integer Size",
    form_name = "text",
    field_type_r = "factor",
    field_choices = c("Small", "Medium", "Large"),
    data_func = function(project) {
      nums <- as.integer(project$data$text$var_text_integer)
      final <- ifelse(nums <= 33, "Small", ifelse(nums <= 66, "Medium", "Large"))
      final # must be in same order as original
    }
  )
  dataset <- project$generate_dataset("custom", exclude_identifiers = FALSE)
  expect_logical(dataset$data$merged$letter_b)
  expect_factor(dataset$data$merged$factor_sml)
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
test_that("add_project_transformation works!", {
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
