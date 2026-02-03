# sync_project ( Exported )
# sync ( Exported )
test_that("sync works!", {
})
test_that("due_for_sync works", {
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
  # true for nonexistent
  expect_true(due_for_sync("NONEXISTENT_PROJECT"))
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  project_details <- extract_project_details(project)
  project_details$last_sync <- NA
  add_project_details_to_cache(project_details)
  #returns TRUE when last_sync is NA
  expect_true(due_for_sync(project$project_name))
  #returns TRUE when sync_frequency is always
  project_details$sync_frequency <- "always"
  project_details$last_sync <- now_time()
  add_project_details_to_cache(project_details)
  expect_true(due_for_sync(project$project_name))
  #returns FALSE when sync_frequency is never
  project_details$sync_frequency <- "never"
  project_details$last_sync <- now_time() - lubridate::ddays(100L)
  add_project_details_to_cache(project_details)
  expect_false(due_for_sync(project$project_name))
  #returns FALSE for hourly when last_sync < 1 hour ago
  project_details$sync_frequency <- "hourly"
  project_details$last_sync <- now_time() - lubridate::dminutes(30L)
  add_project_details_to_cache(project_details)
  expect_false(due_for_sync(project$project_name))
  #returns TRUE for hourly when last_sync >= 1 hour ago
  project_details$last_sync <- now_time() - lubridate::dminutes(30L)
  add_project_details_to_cache(project_details)
  expect_false(due_for_sync(project$project_name))
  #returns FALSE for daily when last_sync < 1 day ago
  project_details$sync_frequency <- "daily"
  project_details$last_sync <- now_time() - lubridate::dhours(12L)
  add_project_details_to_cache(project_details)
  expect_false(due_for_sync(project$project_name))
  #returns TRUE for daily when last_sync >= 1 day ago
  project_details$last_sync <- now_time() - lubridate::ddays(2L)
  add_project_details_to_cache(project_details)
  expect_true(due_for_sync(project$project_name))
  #returns FALSE for weekly when last_sync < 1 week ago
  project_details$sync_frequency <- "weekly"
  project_details$last_sync <- now_time() - lubridate::ddays(3L)
  add_project_details_to_cache(project_details)
  expect_false(due_for_sync(project$project_name))
  #returns TRUE for weekly when last_sync >= 1 week ago
  project_details$last_sync <- now_time() - lubridate::dweeks(2L)
  add_project_details_to_cache(project_details)
  expect_true(due_for_sync(project$project_name))
  #returns FALSE for monthly when last_sync < 1 month ago
  project_details$sync_frequency <- "monthly"
  project_details$last_sync <- now_time() - lubridate::ddays(15L)
  add_project_details_to_cache(project_details)
  expect_false(due_for_sync(project$project_name))
  #returns TRUE for monthly when last_sync >= 1 month ago
  project_details$last_sync <- now_time() - lubridate::dmonths(2L)
  add_project_details_to_cache(project_details)
  expect_true(due_for_sync(project$project_name))
  #returns FALSE for once when already synced
  project_details$sync_frequency <- "once"
  project_details$last_sync <- now_time()
  add_project_details_to_cache(project_details)
  expect_false(due_for_sync(project$project_name))
  #returns FALSE for once when already synced
  project_details$last_sync <- now_time()
  add_project_details_to_cache(project_details)
  expect_false(due_for_sync(project$project_name))
  #returns TRUE for once when never synced
  project_details$last_sync <- NA
  add_project_details_to_cache(project_details)
  expect_true(due_for_sync(project$project_name))
})
# sweep_dirs_for_cache ( Internal )
test_that("sweep_dirs_for_cache updates cache when project files exist", {
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
  # Setup initial project
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  project_details <- extract_project_details(project)
  add_project_details_to_cache(project_details)
  # Create project files on disk
  project_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path
  )
  dir.create(dirname(project_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(project, file = project_path)
  details_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path,
    type = "details"
  )
  saveRDS(project_details, file = details_path)
  # Run sweep
  expect_no_error(sweep_dirs_for_cache())
  # Cache should still contain the project
  projects <- get_projects()
  expect_true(project$project_name %in% projects$project_name)
})
test_that("sweep_dirs_for_cache handles empty cache", {
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
  projects_before <- get_projects()
  expect_identical(nrow(projects_before), 0L)
  # Sweep should handle empty cache gracefully
  expect_no_error(sweep_dirs_for_cache())
  projects_after <- get_projects()
  expect_identical(nrow(projects_after), 0L)
})
test_that("sweep_dirs_for_cache handles non-existent project_names", {
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
  # Setup one project
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  project_details <- extract_project_details(project)
  add_project_details_to_cache(project_details)
  # Try to sweep with non-existent project names
  expect_no_error(sweep_dirs_for_cache(
    project_names = "NONEXISTENT_PROJECT"
  ))
  # Original project should still be in cache
  projects <- get_projects()
  expect_true(project$project_name %in% projects$project_name)
})
test_that("sweep_dirs_for_cache compares cached and disk project details", {
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
  # Setup initial project
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  project_details <- extract_project_details(project)
  add_project_details_to_cache(project_details)
  # Create project files with updated details
  project_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path
  )
  dir.create(dirname(project_path), showWarnings = FALSE, recursive = TRUE)
  # Modify the project details
  project$redcap$version <- "13.0.0"
  updated_project_details <- extract_project_details(project)
  details_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path,
    type = "details"
  )
  saveRDS(updated_project_details, file = details_path)
  # Save full project
  saveRDS(project, file = project_path)
  # Run sweep
  expect_no_error(sweep_dirs_for_cache())
  # Cache should be updated with new version
  projects <- get_projects()
  cached_project <- projects[
    which(projects$project_name == project$project_name),
  ]
  expect_identical(cached_project$version, "13.0.0")
})
