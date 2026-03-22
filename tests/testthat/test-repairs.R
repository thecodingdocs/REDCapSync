tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
# repair_project_details (Internal)
test_that("repair_project_details works with valid project file", {
  project <- mock_test_project()$.internal
  # Save project to disk
  project_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path
  )
  saveRDS(project, file = project_path)
  # Create project_details with matching project_name and dir_path
  project_details <- data.frame(
    project_name = project$project_name,
    dir_path = project$dir_path,
    redcap_uri = project$links$redcap_uri,
    stringsAsFactors = FALSE
  )
  result <- repair_project_details(project_details)
  expect_s3_class(result, "data.frame")
  expect_identical(result$project_name, project$project_name)
  expect_identical(result$dir_path, project$dir_path)
})
test_that("repair_project_details returns NULL when project file missing", {
  project_details <- data.frame(
    project_name = "NONEXISTENT",
    dir_path = file.path("nonexistent", "path"),
    stringsAsFactors = FALSE
  )
  result <- repair_project_details(project_details)
  expect_null(result)
})
test_that("repair_project_details returns NULL when dir_path is NA", {
  project_details <- data.frame(
    project_name = "TEST",
    dir_path = NA_character_,
    stringsAsFactors = FALSE
  )
  result <- repair_project_details(project_details)
  expect_null(result)
})
test_that("repair_project_details handles corrupted project file", {
  project <- mock_test_project()$.internal
  project_details_df <- data.frame(
    project_name = "CORRUPT_PROJECT",
    dir_path = project$dir_path,
    stringsAsFactors = FALSE
  )
  # Create corrupted project file
  project_path <- get_project_path(
    project_name = "CORRUPT_PROJECT",
    dir_path = project$dir_path
  )
  saveRDS("not a valid project object", file = project_path)
  result <- repair_project_details(project_details_df)
  expect_null(result)
})
test_that("repair_project_details extracts correct project details", {
  project <- mock_test_project()$.internal
  # Save project to disk
  project_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path
  )
  saveRDS(project, file = project_path)
  project_details <- data.frame(
    project_name = project$project_name,
    dir_path = project$dir_path,
    stringsAsFactors = FALSE
  )
  result <- repair_project_details(project_details)
  # Verify extracted details match original project
  expect_identical(result$project_name, project$project_name)
  expect_identical(result$project_id, project$redcap$project_id)
  expect_identical(result$token_name, project$token_name)
  expect_identical(result$redcap_uri, project$links$redcap_uri)
})
# repair_projects (Internal)
test_that("repair_projects works with valid project details", {
  project <- mock_test_project()$.internal
  project_path <- get_project_path(
    project_name = project$project_name,
    dir_path = project$dir_path
  )
  saveRDS(project, file = project_path)
  # Create valid project details
  project_details <- extract_project_details(project)
  projects <- project_details
  # Test repair_projects with valid data
  result <- repair_projects(projects)
  expect_s3_class(result, "data.frame")
  expect_true(test_project_details(result))
  expect_identical(nrow(result), 1L)
  expect_identical(result$project_name, project$project_name)
})
test_that("repair_projects returns blank when given empty data frame", {
  projects <- data.frame()
  result <- repair_projects(projects)
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 0L)
})
test_that("repair_projects returns NULL for non-data.frame input", {
  result <- repair_projects("not a data frame")
  expect_null(result)
})
test_that("repair_projects returns NULL when missing required columns", {
  projects <- data.frame(
    project_name = "TEST",
    stringsAsFactors = FALSE
  )
  result <- repair_projects(projects)
  expect_null(result)
})
test_that("repair_projects handles missing project files", {
  projects <- data.frame(
    project_name = "NONEXISTENT",
    redcap_uri = "https://redcap.fake.edu/api/",
    token_name = "REDCAPSYNC_NONEXISTENT",
    project_id = "9999",
    dir_path = file.path("nonexistent", "path"),
    stringsAsFactors = FALSE
  )
  expect_message(
    repair_projects(projects),
    "Cannot extract project details"
  )
})
# repair_setup_project (Internal)
test_that("repair_setup_project works with valid project", {
  project <- mock_test_project()$.internal
  result <- repair_setup_project(project)
  expect_list(result)
  expect_true(test_setup_project(result))
  expect_identical(result$project_name, project$project_name)
})
test_that("repair_setup_project returns NULL for non-list input", {
  result <- repair_setup_project("not a list")
  expect_null(result)
})
test_that("repair_setup_project returns NULL when missing required fields", {
  project <- list(
    project_name = "TEST",
    dir_path = file.path("test", "path")
    # missing: redcap, metadata, data, internals
  )
  result <- repair_setup_project(project)
  expect_null(result)
})
test_that("repair_setup_project returns NULL for invalid project_name", {
  project <- mock_test_project()$.internal
  project$project_name <- "invalid name with spaces"
  result <- repair_setup_project(project)
  expect_null(result)
})
test_that("repair_setup_project repairs invalid internals", {
  project <- mock_test_project()$.internal
  # Set invalid internals
  project$settings$sync_frequency <- "invalid"
  project$settings$labelled <- NA
  project$redcap$timezone <- "Invalid_Zone"
  result <- repair_setup_project(project)
  expect_true(test_setup_project(result))
  # Check that defaults were applied
  expect_identical(result$settings$sync_frequency, "daily")
  expect_true(result$settings$labelled)
  expect_identical(result$redcap$timezone, Sys.timezone())
})
test_that("repair_setup_project repairs invalid batch sizes", {
  project <- mock_test_project()$.internal
  # Set invalid batch sizes
  project$settings$batch_size_download <- 0L
  project$settings$batch_size_upload <- NA
  result <- repair_setup_project(project)
  expect_true(test_setup_project(result))
  # Check that defaults were applied
  expect_identical(result$settings$batch_size_download, 2000L)
  expect_identical(result$settings$batch_size_upload, 500L)
})
test_that("repair_setup_project repairs invalid logical fields", {
  project <- mock_test_project()$.internal
  # Set invalid logical fields
  project$settings$get_files <- NA
  project$settings$get_file_repository <- "yes"
  project$settings$get_entire_log <- 1L
  result <- repair_setup_project(project)
  expect_true(test_setup_project(result))
  # Check that defaults were applied
  expect_false(result$settings$get_files)
  expect_false(result$settings$get_file_repository)
  expect_false(result$settings$get_entire_log)
})
test_that("repair_setup_project repairs invalid get_type", {
  project <- mock_test_project()$.internal
  project$settings$get_type <- "invalid_type"
  result <- repair_setup_project(project)
  expect_true(test_setup_project(result))
  expect_identical(result$settings$get_type, "identified")
})
test_that("repair_setup_project handles character vector fields", {
  project <- mock_test_project()$.internal
  # Set valid character vectors
  project$settings$records <- c("1", "2", "3")
  project$settings$fields <- c("field1", "field2")
  project$settings$forms <- c("form1", "form2")
  result <- repair_setup_project(project)
  expect_true(test_setup_project(result))
  expect_identical(result$settings$records, c("1", "2", "3"))
  expect_identical(result$settings$fields, c("field1", "field2"))
  expect_identical(result$settings$forms, c("form1", "form2"))
})
test_that("repair_setup_project repairs invalid character vectors", {
  project <- mock_test_project()$.internal
  # Set invalid character vectors (wrong type or length)
  project$settings$records <- 123L
  project$settings$fields <- TRUE
  project$settings$forms <- NA
  result <- repair_setup_project(project)
  expect_true(test_setup_project(result))
  # Check that defaults (NA) were applied
  expect_true(is.na(result$settings$records))
  expect_true(is.na(result$settings$fields))
  expect_true(is.na(result$settings$forms))
})
test_that("repair_setup_project repairs invalid log_days", {
  project <- mock_test_project()$.internal
  project$settings$log_days <- 0L
  result <- repair_setup_project(project)
  expect_true(test_setup_project(result))
  expect_identical(result$settings$log_days, 10L)
})
