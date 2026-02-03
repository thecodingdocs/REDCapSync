# get_projects ( Exported )
test_that("get_projects is df and has appropriate columns", {
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
  df <- get_projects()
  expect_s3_class(df, "data.frame")
  expect_true(all(colnames(df) %in% .blank_project_cols))
  df <- .blank_project_details
  expect_s3_class(df, "data.frame")
  expect_true(all(colnames(df) %in% .blank_project_cols))
})
# extract_project_details ( Internal )
test_that("extract_project_details works", {
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
  project <- load_test_project()$.internal
  project_details <- extract_project_details(project)
  expect_data_frame(project_details,
                    ncols = ncol(.blank_project_details),
                    nrows = 1L)
  expect_identical(colnames(project_details),
                   colnames(.blank_project_details))
})
# save_projects_to_cache ( Internal )
test_that("save_projects_to_cache works", {
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
  projects <- get_projects()
  expect_data_frame(projects, nrows = 0L)
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  #no connection should not save to cache
  project_details <- extract_project_details(project)
  projects <- get_projects()
  expect_data_frame(projects, nrows = 0L)
  project$internals$ever_connected <- TRUE
  save_project(project = project)
  projects <- get_projects()
  #connection should save to cache
  expect_data_frame(projects, nrows = 1L)
  expect_identical(project_details$project_name, projects$project_name)
  expect_data_frame(project_details, nrows = 1L)
  cache_clear()
  expect_false(file.exists(file.path(fake_cache_location, "projects.rds")))
  projects <- get_projects()
  expect_data_frame(projects, nrows = 0L)
  expect_message(save_projects_to_cache(project_details, silent = FALSE),
                 "saved")
  expect_true(file.exists(file.path(fake_cache_location, "projects.rds")))
  expect_no_message(save_projects_to_cache(project_details, silent = TRUE))
})
# blank_tibble ( Internal )
# na_if_null ( Internal )
# add_project_details_to_cache ( Internal )
test_that("add_project_details_to_cache works", {
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
  # start empty
  projects <- get_projects()
  expect_data_frame(projects, nrows = 0L)
  # add project details to cache
  project <- load_test_project()$.internal
  project_details <- extract_project_details(project)
  expect_no_error(add_project_details_to_cache(project_details))
  projects <- get_projects()
  expect_data_frame(projects, nrows = 1L)
  expect_true(project_details$project_name %in% projects$project_name)
  # conflict: same project_id & same base redcap_uri
  #but different project_name -> error
  project_details_conflict <- project_details
  project_details_conflict$project_name <- "TEST_OTHER"
  expect_error(add_project_details_to_cache(project_details_conflict))
})
# save_project_details ( Internal )
# test_that("save_project_details works", {
#   test_dir <- withr::local_tempdir() |> sanitize_path()
#   fake_cache_location <- file.path(test_dir, "fake_cache")
#   local_mocked_bindings(
#     get_cache = function(...) {
#       fake_cache <- hoardr::hoard()
#       fake_cache$cache_path_set(full_path = fake_cache_location)
#       fake_cache$mkdir()
#       fake_cache
#     }
#   )
#   project <- load_test_project()$.internal
#   project$dir_path <- set_dir(test_dir)
#   # ensure details path does not exist initially
#   details_path <- get_project_path2(project, type = "details")
#   expect_false(file.exists(details_path))
#   # call function
#   expect_no_error(save_project_details(project))
#   # details file created and contains expected project_name
#   expect_true(file.exists(details_path))
#   saved_details <- readRDS(details_path)
#   expect_identical(
#  as.character(saved_details$project_name), project$project_name)
#   # cache updated
#   projects <- get_projects()
#   expect_true(project$project_name %in% projects$project_name)
# })
