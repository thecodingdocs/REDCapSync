test_that("test_dir works", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "projects.rds")
  expect_false(file.exists(test_file))
  file.create(test_file)
  expect_true(file.exists(test_file))
})
# setup_project ( Exported )
test_that("setup_project creates a valid project object and valid directory", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  expect_error(assert_dir(dir_path = test_dir))
  project_name <- "TEST_PROJECT"
  redcap_uri <- "https://redcap.miami.edu/api/"
  # test_short_names
  expect_error(assert_env_name("A project"))
  expect_error(assert_env_name("project$]"))
  expect_error(assert_env_name("1"))
  expect_error(assert_env_name(1L))
  expect_error(assert_env_name(another_name))
  expect_no_error(assert_env_name("expected_name"))
  expect_no_error(assert_env_name("expected_name2"))
  expect_no_error(assert_env_name("EXPECTED_NAME"))
  # test_redcap_base
  expect_error(assert_web_link("https://redcap.blah"))
  expect_no_error(assert_web_link("https://redcap.miami.edu/"))
  expect_no_error(assert_web_link("https://redcap.miami.edu"))
  expect_no_error(assert_web_link("https://redcap.edu"))
  # test db
  expect_no_error(assert_blank_project(.blank_project))
  expect_error(assert_setup_project(.blank_project))
  expect_error(assert_blank_project(.blank_project[[-1L]]))
  expect_error(assert_blank_project(1L))
  expect_error(assert_blank_project(data.frame()))
  expect_error(assert_dir(project$dir_path))
  # Run setup_project
  project <- setup_project(
    project_name = project_name,
    dir_path = test_dir,
    redcap_uri = redcap_uri,
    hard_reset = TRUE
  )$.internal # change to R6 later
  expect_no_error(assert_dir(dir_path = test_dir))
  expect_no_error(assert_blank_project(project = project))
  # expect_no_error(get_dir(project))
  check_dir <- assert_dir(project$dir_path)
  expect_identical(test_dir, check_dir)
  expect_type(project,type = "list")
  expect_named(project)
  expect_true("project_name" %in% names(project))
  expect_true("dir_path" %in% names(project))
  expect_false(project$internals$is_blank)
  expect_false(project$internals$is_test)
  expect_true(file.exists(project$dir_path))
  expect_identical(project$project_name, project_name)
  test_dir_files <- list.files(test_dir)
  expect_true(all(.dir_folders %in% test_dir_files))
  project$dir_path <- file.path(test_dir, "another_fake_folder") |>
    sanitize_path()
  expect_error(assert_dir(project$dir_path))
})
# load_project ( Exported )
# load_test_project ( Exported )
test_that("load_project works", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  expect_error(assert_dir(test_dir))
})
# save_project ( Exported )
test_that("save_project doesn't save if blank but will save if valid", {
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
  project_name <- "TEST_PROJECT"
  redcap_uri <- "https://redcap.miami.edu/api/"
  project <- setup_project(project_name = project_name,
                           dir_path = test_dir,
                           redcap_uri = redcap_uri)$.internal # change to R6
  save_project(project)
  expect_false(file.exists(file.path(
    project$dir_path,
    "R_objects",
    paste0(project_name, "_REDCapSync.RData")
  )))
  project$internals$ever_connected <- TRUE
  fake_time <- now_time()
  project$internals$last_sync <- fake_time
  project$internals$last_directory_save <- fake_time
  project$internals$last_data_update <- fake_time
  project$internals$last_metadata_update <- fake_time
  project$internals$timezone <- Sys.timezone()
  project <- save_project(project)
  expected_save_location <- file.path(project$dir_path,
                                      "R_objects",
                                      paste0(project_name, "_REDCapSync.RData"))
  expect_true(file.exists(expected_save_location))
  # check cached proj
  projects <- get_projects()
  expect_identical(nrow(get_projects()), 1L)
  expect_identical(projects$project_name, project_name)
  expect_identical(projects$redcap_uri, redcap_uri)
  expect_identical(projects$dir_path, test_dir)
  # loading tests wont load unknown project
  expect_error(load_project("a_project"))
  # loads what we saved
  # change to R6 later
  project2 <- load_project(project_name = project_name)$.internal
  # project$internals$last_directory_save |> attr("tzone")
  # project2$internals$last_directory_save |> attr("tzone")
  expect_identical(project, project2)
  # # delete_project works...
  # expect_no_warning(delete_project(project))
  # expect_warning(delete_project(project)) # warning for deleting twice
  # expect_error(load_project(project_name = project_name)) # wont load deleted
})
# set_dir ( Internal )
test_that("set_dir creates a new directory if it does not exist", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  dir_path <- file.path(test_dir, "new_dir")
  # Mock user input to create the directory
  mockery::stub(set_dir, "utils::menu", 1L)
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_true(file.exists(dir_path))
  expect_true(all(.dir_folders %in% list.files(dir_path)))
})
test_that("set_dir handles existing directory correctly", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  dir_path <- file.path(test_dir, "existing_dir")
  dir.create(dir_path)
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_true(file.exists(dir_path))
  expect_true(all(.dir_folders %in% list.files(dir_path)))
})
test_that("set_dir throws an error for invalid directory path", {
  expect_error(set_dir(123L), "dir must be a character string")
})
test_that("set_dir creates missing internal directories", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  dir_path <- file.path(test_dir, "partial_dir")
  dir.create(dir_path)
  dir.create(file.path(dir_path, "R_objects"))
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_true(file.exists(dir_path))
  expect_true(all(.dir_folders %in% list.files(dir_path)))
})
test_that("set_dir stops if user chooses not to create directory", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  dir_path <- file.path(test_dir, "no_create_dir")
  # Mock user input to not create the directory
  mockery::stub(set_dir, "utils::menu", 2L)
  expect_error(set_dir(dir_path), "Path not found. Use absolute path")
  expect_false(file.exists(dir_path))
})
test_that("set_dir validates the directory structure", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  dir_path <- file.path(test_dir, "valid_dir")
  dir.create(dir_path)
  for (folder in .dir_folders) {
    dir.create(file.path(dir_path, folder))
  }
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_true(file.exists(dir_path))
  expect_true(all(.dir_folders %in% list.files(dir_path)))
})
# clean_dir_path ( Internal )
