## test-get_projects
test_that("get_projects is df and has appropriate columns", {
  df <- get_projects()
  expect_s3_class(df, "data.frame")
  expect_true(all(colnames(df) %in% .blank_project_cols))
  df <- .blank_project_details
  expect_s3_class(df, "data.frame")
  expect_true(all(colnames(df) %in% .blank_project_cols))
})
test_that("extract_project_details works", {
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
  project_details <- extract_project_details(project)
  checkmate::expect_data_frame(project_details,
                               ncols = ncol(.blank_project_details),
                               nrows = 1)
  expect_equal(colnames(project_details), colnames(.blank_project_details))
})
test_that("extract_project_details works", {
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
  project_details <- extract_project_details(project)
  checkmate::expect_data_frame(project_details,
                               ncols = ncol(.blank_project_details),
                               nrows = 1)
  expect_equal(colnames(project_details), colnames(.blank_project_details))
})
test_that("save_projects_to_cache works", {
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
  projects <- get_projects()
  checkmate::expect_data_frame(projects, nrows = 0)
  project <- mock_project()
  project_details <- extract_project_details(project)
  projects <- get_projects()
  checkmate::expect_data_frame(projects, nrows = 1)
  expect_equal(project_details$short_name, projects$short_name)
  checkmate::expect_data_frame(project_details, nrows = 1)
  cache_clear()
  expect_false(file.exists(file.path(fake_cache_location,"projects.rds")))
  projects <- get_projects()
  checkmate::expect_data_frame(projects, nrows = 0)
  expect_message(save_projects_to_cache(project_details , silent = FALSE),
                 "saved")
  expect_true(file.exists(file.path(fake_cache_location,"projects.rds")))
  expect_no_message(save_projects_to_cache(project_details , silent = TRUE))
})
test_that("cache_remove_project works", {
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
  projects <- get_projects()
  project_details <- extract_project_details(project)
  expect_message(cache_remove_project(short_name = "TEST_other"),
                 "Nothing to do")
  expect_true("TEST_PROJECT" %in% projects$short_name)
  expect_false("TEST_other" %in% projects$short_name)
  expect_message(cache_remove_project(short_name = "TEST_other"),
                 "Nothing to do")
  expect_message(cache_remove_project(short_name = "TEST_PROJECT"),
                 "removed from cache")
  projects <- get_projects()
  expect_false("TEST_PROJECT" %in% projects$short_name)
})
# test_that("check_folder_for_projects works", {
#   test_dir <- withr::local_tempdir() %>% sanitize_path()
#   test_dir_folder <- file.path(test_dir, "R_objects")
#   test_file1 <- file.path(test_dir, "R_objects", "not_a_proj.RData")
#   test_file2 <- file.path(test_dir, "R_objects", "PROJ_REDCapSync.RData")
#   test_file3 <- file.path(test_dir, "ANOTHER_PROJ_REDCapSync.RData")
#   test_file4 <- file.path(test_dir, "ANOTHER_PROJ_wrong_suffix.RData")
#   dir.create(test_dir_folder)
#   file.create(test_file1)
#   file.create(test_file2)
#   file.create(test_file3)
#   file.create(test_file4)
#   local_mocked_bindings(
#     assert_dir = function(...) test_dir
#   )
#   # check without validation
#   expect_false(test_file1 %in% check_folder_for_projects(file_path = test_dir, validate = FALSE))
#   expect_contains(check_folder_for_projects(file_path = test_dir, validate = FALSE), test_file2)
#   expect_contains(check_folder_for_projects(file_path = test_dir, validate = FALSE), test_file3)
#   expect_false(test_file4 %in% check_folder_for_projects(file_path = test_dir, validate = FALSE))
#   # check validation
#   expect_false(test_file1 %in% check_folder_for_projects(file_path = test_dir, validate = TRUE))
#   expect_contains(check_folder_for_projects(file_path = test_dir, validate = TRUE), test_file2)
#   expect_false(test_file3 %in% check_folder_for_projects(file_path = test_dir, validate = TRUE))
#   expect_false(test_file4 %in% check_folder_for_projects(file_path = test_dir, validate = TRUE))
# })
