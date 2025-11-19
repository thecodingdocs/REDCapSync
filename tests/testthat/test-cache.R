test_that("hoardr cache exsists", {
  expect_true(file.exists(cache_path()))
  expect_true(cache_exists())
})
# get_cache ( Internal )
test_that("fake_cache sets and clears", {
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
  expect_false(file.exists(fake_cache_location))
  fake_cache <- get_cache()
  expect_true(file.exists(fake_cache_location))
  expect_true(file.exists(fake_cache$cache_path_get()))
  expect_equal(fake_cache$cache_path_get(), fake_cache_location)
  test_file <- file.path(fake_cache$cache_path_get(), "projects.rds")
  expect_false(file.exists(test_file))
  file.create(test_file)
  expect_true(file.exists(test_file))
  fake_cache$delete_all()
  expect_false(file.exists(test_file))
})
# cache_clear ( Exported )
# cache_projects_exists ( Internal )
test_that("cache_projects_exists, cache_clear works", {
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
  expect_false(file.exists(fake_cache_location))
  fake_cache <- get_cache()
  expect_true(file.exists(fake_cache_location))
  expect_true(file.exists(fake_cache$cache_path_get()))
  expect_equal(fake_cache$cache_path_get(), fake_cache_location)
  test_file <- file.path(fake_cache$cache_path_get(), "projects.rds")
  expect_false(cache_projects_exists())
  file.create(test_file)
  expect_true(cache_projects_exists())
  expect_no_error(cache_clear())
  expect_false(cache_projects_exists())
  local_mocked_bindings(
    cache_exists = function(...) FALSE
  )
  expect_false(cache_projects_exists())
})
# cache_path ( Exported )
# cache_exists ( Internal )
# cache_remove_project ( Exported )
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
