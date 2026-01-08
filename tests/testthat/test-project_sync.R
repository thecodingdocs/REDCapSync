# sync_project ( Exported )
test_that("sync_project works!", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      fake_cache
    },
    due_for_sync = function(...) {FALSE}
  )
  project <- mock_project()
  expect_message({sync_project(project)},"not due for sync")
})
# sync_all ( Exported )
test_that("sync works!", {
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
  project <- mock_project()
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
  project <- mock_project()
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
  project <- mock_project()
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
