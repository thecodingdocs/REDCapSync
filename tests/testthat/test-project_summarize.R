test_that("fields_to_choices works", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      return(fake_cache)
    }
  )
  project <- mock_project()
  fields <- project$metadata$fields
  fields <- fields %>% dplyr::bind_rows(
    data.frame(
      field_name = c(
        "sex",
        "yes_no1",
        "yes_no2",
        "race",
        "location"
      ),
      form_name = "form_one",
      field_type = c(
        "radio",
        "yesno",
        "yesno",
        "dropdown",
        "checkbox"
      ),
      select_choices_or_calculations = c(
        "0, Female | 1, Male", # 2
        "0, No | 1, Yes", # 2
        "0,No | 1,Yes",# 2
        "0, American Indian/Alaska Native | 1, Asian | 2, Native Hawaiian or Other Pacific Islander | 3, Black or African American | 4, White | 5, More Than One Race | 6, Unknown / Not Reported",# 7
        "1, Lung | 2, Liver | 3, Bone | 4, Lymph node | 5, Other | 6, Brain" #6
      )
    )
  )
  fields <- add_field_elements(fields)
  sum_of_choices <- 2 + 2 + 2 + 7 + 6*(2) + 3
  choices <- fields_to_choices(fields)
  checkmate::expect_data_frame(choices,nrows = sum_of_choices)
  expect_false(any(is.na(choices$code)))
  expect_false(any(is.na(choices$name)))
  expect_false(any(is.na(choices$field_name)))
  expect_false(any(is.na(choices$form_name)))
  should_be_unique <- paste0(choices$field_name,"_",choices$code)
  #check warning for same name different code here?
  expect_false(anyDuplicated(should_be_unique)>0)
})
