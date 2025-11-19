# get_projects ( Exported )
test_that("get_projects is df and has appropriate columns", {
  df <- get_projects()
  expect_s3_class(df, "data.frame")
  expect_true(all(colnames(df) %in% .blank_project_cols))
  df <- .blank_project_details
  expect_s3_class(df, "data.frame")
  expect_true(all(colnames(df) %in% .blank_project_cols))
})
# extract_project_details ( Internal )
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
# save_projects_to_cache ( Internal )
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
# blank_tibble ( Internal )
# na_if_null ( Internal )
# add_project_details_to_cache ( Internal )
# add_project_details_to_project ( Internal )
# save_project_details ( Internal )
