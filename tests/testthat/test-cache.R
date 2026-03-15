tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
test_that("local_envvar seen by tests, exists, but empty at first", {
  withr::local_envvar(REDCAPSYNC_CONFIG_CACHE_DIR = tempdir_file)
  expect_true(nzchar(Sys.getenv("R_USER_CACHE_DIR")))
  expect_directory_exists(Sys.getenv("R_USER_CACHE_DIR"))
  expect_directory_exists(Sys.getenv("REDCAPSYNC_CONFIG_CACHE_DIR"))
  testing_path <- Sys.getenv("R_USER_CACHE_DIR") |> file.path("R", "REDCapSync")
  testing_path2 <- Sys.getenv("REDCAPSYNC_CONFIG_CACHE_DIR")
  real_cache_path_projects <- file.path(testing_path, "projects.rds")
  real_cache_path_projects2 <- file.path(testing_path2, "projects.rds")
  expect_false(test_directory_exists(testing_path))
  expect_directory_exists(testing_path2)
  expect_false(test_file_exists(real_cache_path_projects))
  expect_false(test_file_exists(real_cache_path_projects2))
})
# cache_clear (Exported)
test_that("cache_clear works!", {
  tempdir_test <- sanitize_path(withr::local_tempdir())
  withr::local_envvar(R_USER_CACHE_DIR = tempdir_test)
  fake_cache <- get_cache()
  test_file <- file.path(fake_cache$cache_path_get(), "projects.rds")
  fake_project_details <- .blank_project_details  |>
    bind_rows(data.frame(project_name = "FAKE1", stringsAsFactors = FALSE)) |>
    bind_rows(data.frame(project_name = "FAKE2", stringsAsFactors = FALSE)) |>
    bind_rows(data.frame(project_name = "FAKE3", stringsAsFactors = FALSE)) |>
    bind_rows(data.frame(project_name = "FAKE4", stringsAsFactors = FALSE))
  saveRDS(fake_project_details, test_file)
  expect_file_exists(test_file)
  projects <- get_projects()
  expect_data_frame(projects, nrows = 4L)
  expect_message(cache_clear("FAKE2"), "removed from cache")
  expect_message(cache_clear("FAKE2"), "not in your cache")
  projects <- get_projects()
  expect_data_frame(projects, nrows = 3L)
  expect_false("FAKE2" %in% projects$project_name)
  expect_message(cache_clear(c("FAKE1", "FAKE4")), "removed from cache")
  expect_message(cache_clear(c("FAKE1", "FAKE4")), "not in your cache")
  projects <- get_projects()
  expect_data_frame(projects, nrows = 1L)
  expect_identical(projects$project_name, "FAKE3")
  expect_file_exists(test_file)
  expect_message(cache_clear(), "cache cleared!")
  expect_message({
    projects <- get_projects()
  }, "No cached projects")
  expect_false(file.exists(test_file))
})
# cache_path (Internal)
test_that("cache_path works inside and outside of testing!", {
  expect_true(nzchar(Sys.getenv("R_USER_CACHE_DIR")))
  testing_path <- Sys.getenv("R_USER_CACHE_DIR") |> file.path("R", "REDCapSync")
  expect_directory_exists(cache_path())
  expect_directory_exists(testing_path)
  expect_identical(cache_path(), testing_path)
  withr::local_envvar(R_USER_CACHE_DIR = NA)
  expect_identical(Sys.getenv("R_USER_CACHE_DIR"), "")
  fake_other_cache <- hoardr::hoard()
  fake_other_cache$cache_path_set(path = "REDCapSync", type = "user_cache_dir")
  expected_user_path <- sanitize_path(fake_other_cache$cache_path_get())
  expect_directory_exists(cache_path())
  expect_directory_exists(expected_user_path)
  expect_identical(cache_path(), expected_user_path)
})
# cache_remove_project (Internal)
test_that("cache_remove_project works!", {
  project <- mock_test_project()$.internal
  projects <- get_projects()
  project_details <- extract_project_details(project)
  expect_message(cache_remove_project(project_name = "TEST_OTHER"),
                 "Nothing to do")
  expect_false("TEST_other" %in% projects$project_name)
  expect_false("TEST_CLASSIC" %in% projects$project_name)
  add_project_details_to_cache(project_details)
  projects <- get_projects()
  expect_true("TEST_CLASSIC" %in% projects$project_name)
  expect_message(cache_remove_project(project_name = "TEST_CLASSIC"),
                 "removed from cache")
  projects <- get_projects()
  expect_false("TEST_CLASSIC" %in% projects$project_name)
})
# get_cache (Internal)
test_that("get_cache works and deletes!", {
  tempdir_test <- sanitize_path(withr::local_tempdir())
  withr::local_envvar(R_USER_CACHE_DIR = tempdir_test)
  test_cache <- get_cache()
  test_cache_path <- test_cache$cache_path_get()
  expect_directory_exists(test_cache_path)
  test_file <- file.path(test_cache_path, "projects.rds")
  expect_false(file.exists(test_file))
  file.create(test_file)
  expect_file_exists(test_file)
  test_cache$delete_all()
  expect_false(file.exists(test_file))
})
# sweep_dirs_for_cache (Internal)
test_that("sweep_dirs_for_cache updates cache when project files exist", {
  project <- mock_test_project()$.internal
  project_details <- extract_project_details(project)
  add_project_details_to_cache(project_details)
  # Create project files on disk
  project_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path
  )
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
  tempdir_test <- sanitize_path(withr::local_tempdir())
  withr::local_envvar(R_USER_CACHE_DIR = tempdir_test)
  projects_before <- get_projects()
  expect_identical(nrow(projects_before), 0L)
  # Sweep should handle empty cache
  expect_no_error(sweep_dirs_for_cache())
  projects_after <- get_projects()
  expect_identical(nrow(projects_after), 0L)
})
test_that("sweep_dirs_for_cache handles non-existent project_names", {
  project <- mock_test_project()$.internal
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
  project <- mock_test_project()$.internal
  project_details <- extract_project_details(project)
  add_project_details_to_cache(project_details)
  # Create project files with updated details
  project_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path
  )
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
