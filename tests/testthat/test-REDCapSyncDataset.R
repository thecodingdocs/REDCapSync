tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
test_that("REDCapSyncDataset object works!", {
  project <- mock_test_project()
  dataset <- project$generate_dataset("custom")
  expect_message(dataset$print(), "Dataset: custom")
  expected_dataset_dir <- file.path(dataset$project_details$dir_path, "output")
  file_name <- "TEST_CLASSIC_custom.xlsx"
  expect_message(dataset$save(), "Saved TEST_CLASSIC_custom.xlsx")
  expect_file_exists(file.path(expected_dataset_dir, file_name))
})
