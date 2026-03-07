withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
# setup_project ( Exported )
test_that("setup_project creates a valid project object and valid directory", {
  withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
  temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  expect_error(assert_dir(dir_path = temp_dir))
  project_name <- "TEST_CLASSIC"
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
  expect_error(assert_dir(temp_dir))
  # Run setup_project
  project <- setup_project(
    project_name = project_name,
    dir_path = temp_dir,
    redcap_uri = redcap_uri,
    hard_reset = TRUE
  )$.internal # change to R6 later
  expect_no_error(assert_dir(dir_path = temp_dir))
  expect_no_error(assert_blank_project(project = project))
  check_dir <- assert_dir(project$dir_path)
  expect_identical(temp_dir, check_dir)
  expect_type(project, type = "list")
  expect_named(project)
  expect_true("project_name" %in% names(project))
  expect_true("dir_path" %in% names(project))
  expect_false(project$internals$is_blank)
  expect_false(project$internals$is_test)
  expect_directory_exists(project$dir_path)
  expect_identical(project$project_name, project_name)
  test_dir_files <- list.files(temp_dir)
  expect_true(all(.dir_folders %in% test_dir_files))
  project$dir_path <- file.path(temp_dir, "another_fake_folder") |>
    sanitize_path()
  expect_error(assert_dir(project$dir_path))
})
test_that("setup_project checks exisiting dir", {
  withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
  temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  expect_error(assert_dir(dir_path = temp_dir))
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  save_project(project)
  redcap_uri <- "https://redcap.miami.edu/api/"
  # Run setup_project
  expect_error(
    setup_project(
      project_name = project_name,
      dir_path = temp_dir,
      redcap_uri = redcap_uri,
      hard_reset = FALSE
    )$.internal # change to R6 later
  )
  redcap_uri <- project$links$redcap_uri
  project_loaded <- setup_project(
    project_name = project_name,
    dir_path = temp_dir,
    redcap_uri = redcap_uri,
    hard_reset = FALSE
  )$.internal # change to R6 later
  expect_identical(project_loaded$project_name, project$project_name)
  expect_identical(project_loaded$redcap$project_id, project$redcap$project_id)
  expect_warning({
    project_loaded <- setup_project(
      project_name = project_name,
      dir_path = temp_dir,
      redcap_uri = redcap_uri,
      labelled = FALSE,
      hard_reset = FALSE
    )$.internal
  }, "The project that was loaded was labelled and you chose raw")
  expect_null(project_loaded$internals$last_data_update)
})
# load_project ( Exported )
# save_project ( Exported )
test_that("save_project doesn't save if blank but will save if valid", {
  temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  project_name <- "TEST_CLASSIC"
  redcap_uri <- "https://redcap.miami.edu/api/"
  project <- setup_project(project_name = project_name,
                           dir_path = temp_dir,
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
  expect_file_exists(expected_save_location)
  # check cached proj
  projects <- get_projects()
  expect_identical(nrow(get_projects()), 1L)
  expect_identical(projects$project_name, project_name)
  expect_identical(projects$redcap_uri, redcap_uri)
  expect_identical(projects$dir_path, temp_dir)
  # loading tests wont load unknown project
  expect_error(load_project("a_project"))
  # loads what we saved
  # change to R6 later
  project2 <- load_project(project_name = project_name)$.internal
  expect_identical(project, project2)
})
# set_dir ( Internal )
test_that("set_dir handles existing directory correctly", {
  temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  dir_path <- file.path(temp_dir, "existing_dir")
  dir.create(dir_path)
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_directory_exists(dir_path)
  expect_true(all(.dir_folders %in% list.files(dir_path)))
})
test_that("set_dir throws an error for invalid directory path", {
  expect_error(set_dir(123L), "dir must be a character string")
})
test_that("set_dir creates missing internal directories", {
  temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  dir_path <- file.path(temp_dir, "partial_dir")
  dir.create(dir_path)
  dir.create(file.path(dir_path, "R_objects"))
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_directory_exists(dir_path)
  expect_true(all(.dir_folders %in% list.files(dir_path)))
})
test_that("set_dir stops if user chooses not to create directory", {
  temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  dir_path <- file.path(temp_dir, "no_create_dir")
  # Mock user input to not create the directory
  expect_error(set_dir(dir_path), "Path not found. Use absolute path")
  expect_false(file.exists(dir_path))
})
test_that("set_dir validates the directory structure", {
  temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  dir_path <- file.path(temp_dir, "valid_dir")
  dir.create(dir_path)
  for (folder in .dir_folders) {
    dir.create(file.path(dir_path, folder))
  }
  expect_message(set_dir(dir_path), "Directory is Valid!")
  expect_directory_exists(dir_path)
  expect_true(all(.dir_folders %in% list.files(dir_path)))
})
# clean_dir_path ( Internal )
