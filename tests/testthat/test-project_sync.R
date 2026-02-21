withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
# sync_project ( Exported )
# sync ( Exported )
test_that("sync works!", {
})
test_that("sync_project_hard_reset works!", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  # project$data <- .blank_project$data
  mockery::stub(sync_project_hard_reset, "get_redcap_metadata", project)
  mockery::stub(sync_project_hard_reset, "update_project_links", project)
  mockery::stub(sync_project_hard_reset, "get_redcap_data", project$data)
  mockery::stub(sync_project_hard_reset, "get_redcap_log", project$redcap$log)
  result <- project
  result$metadata <- .blank_project$metadata
  result["data"] <- list(NULL)
  result <- sync_project_hard_reset(result)
  expect_identical(result$metadata, project$metadata)
  expect_identical(result$data, project$data)
})
test_that("sync_project_check cannot connect!", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  project$internals$last_test_connection_outcome <- FALSE
  mockery::stub(sync_project_check, "test_project_token", project)
  expect_message(sync_project_check(project), "Could not connect")
})
test_that("sync_project_check wont update if nothing new", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  # Setup: ensure project is ready for sync_project_check
  project$internals$last_test_connection_outcome <- TRUE
  project$internals$last_metadata_update <- now_time() - lubridate::ddays(100L)
  project$internals$last_data_update <- now_time() - lubridate::ddays(100L)
  project$internals$was_updated <- FALSE
  interim_log <- head(project$redcap$log, n = 0L)
  # Mock the test_project_token to simulate successful connection
  mockery::stub(sync_project_check, "test_project_token", project)
  mockery::stub(sync_project_check, "get_interim_log", interim_log)
  # Call sync_project_check with hard_reset = FALSE
  result <- sync_project_check(project, hard_reset = FALSE)
  expect_false(result$internals$was_updated)
})
test_that("sync_project_check will update if new", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  project$internals$last_test_connection_outcome <- TRUE
  project$internals$last_metadata_update <- now_time() - lubridate::ddays(100L)
  project$internals$last_data_update <- now_time() - lubridate::ddays(100L)
  project$internals$was_updated <- FALSE
  interim_log <- head(project$redcap$log, n = 10L)
  ordered_records <- project$summary$all_records$record_id
  row_for_one <- which(project$data$text$record_id == "1")
  row_for_two <- which(project$data$text$record_id == "2")
  var_text_integer_one <- project$data$text$var_text_integer[row_for_one]
  var_text_integer_two <- project$data$text$var_text_integer[row_for_two]
  log_changes <-  analyze_log(interim_log, id_col = project$metadata$id_col)
  updated_records <- log_changes$updated_records
  expect_contains(updated_records, "1")
  expect_false("2" %in% updated_records)
  new_data <- filter_data_list(
    data_list = project,
    filter_field = "record_id",
    filter_choices = updated_records
  )
  row_for_one <- which(new_data$text$record_id == "1")
  row_for_two <- which(new_data$text$record_id == "2")
  expect_length(row_for_one, 1)
  expect_length(row_for_two, 0)
  new_var_text_integer <- new_data$text$var_text_integer[row_for_one]
  expect_identical(new_var_text_integer, var_text_integer_one)
  new_data$text$var_text_integer[row_for_one] <- "99"
  new_var_text_integer <- new_data$text$var_text_integer[row_for_one]
  expect_true(new_var_text_integer != var_text_integer_one)
  # Mock the test_project_token to simulate successful connection
  local_mocked_bindings(
    test_project_token =  function(...) project,
    get_interim_log =  function(...) interim_log,
    get_redcap_data =  function(...) new_data,
    get_redcap_records = function(...) ordered_records,
    due_for_sync = function(...) TRUE
  )
  # Call sync_project_check with hard_reset = FALSE
  result <- sync_project(
    project = project,
    summarize = FALSE,
    save_to_dir = FALSE,
    hard_reset = FALSE
  )
  row_for_one <- which(result$data$text$record_id == "1")
  row_for_two <- which(result$data$text$record_id == "2")
  updated_var_text_integer_one <- result$data$text$var_text_integer[row_for_one]
  updated_var_text_integer_two <- result$data$text$var_text_integer[row_for_two]
  expect_identical(new_var_text_integer, updated_var_text_integer_one)
  expect_identical(var_text_integer_two, updated_var_text_integer_two)
  # expect_false(result$internals$was_updated)
})
test_that("due_for_sync works", {
  # true for nonexistent
  expect_true(due_for_sync("NONEXISTENT_PROJECT"))
  project <- mock_test_project()$.internal
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
test_that("get_interim_log works", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  interim_log <- utils::head(project$redcap$log, n = 10L)
  # Update record 3
  project$redcap$has_log_access <- FALSE
  expect_message(get_interim_log(project), "You do not have logging access")
  project$redcap$has_log_access <- TRUE
  mockery::stub(get_interim_log, "get_redcap_log", interim_log)
  expect_data_frame(get_interim_log(project), nrows = 0L)
  new_interim_log_info <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Create record (API) 99",
    details = NA,
    record = "99",
    action_type = "Create"
  )
  interim_log <- new_interim_log_info |> bind_rows(interim_log)
  mockery::stub(get_interim_log, "get_redcap_log", interim_log)
  extracted_interim <- get_interim_log(project)
  expect_data_frame(extracted_interim, nrows = 1L)
  expect_identical(new_interim_log_info, extracted_interim)
})
test_that("analyze_log works", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  id_col <- project$metadata$id_col
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Create record (API) 99",
    details = NA,
    record = "99",
    action_type = "Create"
  )
  expect_identical(analyze_log(interim_log, id_col)$length_updated_records, 1L)
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Create record (API) 99",
    details = NA,
    record = "99",
    action_type = c("Create","Update")
  )
  expect_identical(analyze_log(interim_log, id_col)$length_updated_records, 1L)
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Create record (API) 99",
    details = NA,
    record = "99",
    action_type = "Delete"
  )
  log_changes <- analyze_log(interim_log, id_col)
  expect_identical(log_changes$length_updated_records, 1L)
  expect_identical(log_changes$length_deleted_records, 1L)
  expect_identical(log_changes$updated_records, "99")
  expect_identical(log_changes$deleted_records, "99")
  expect_true(log_changes$refresh_data)
  expect_false(log_changes$refresh_metadata)
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Changes to metadata",
    details = NA,
    record = NA,
    action_type = "Metadata Change Major"
  )
  expect_true(analyze_log(interim_log, id_col)$refresh_metadata)
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Changes to metadata",
    details = NA,
    record = NA,
    action_type = "Metadata Change Minor"
  )
  expect_true(analyze_log(interim_log, id_col)$refresh_metadata)
})
test_that("log_change_messages works", {
  log_changes <- list(
    hard_reset = FALSE,
    refresh_metadata = FALSE,
    refresh_metadata_major = FALSE,
    refresh_metadata_minor = FALSE,
    refresh_users = FALSE,
    refresh_data = FALSE,
    deleted_records = NULL,
    updated_records = NULL,
    renamed_records = NULL,
    comment_records = NULL,
    length_deleted_records = 0,
    length_updated_records = 0,
    length_renamed_records = 0,
    length_comment_records = 0
  )
  expect_message(log_change_messages(log_changes), "Up to date")
  log_changes$refresh_metadata <- TRUE
  expect_message(log_change_messages(log_changes), "Full update triggered")
  log_changes$refresh_metadata <- FALSE
  log_changes$renamed_records <- "1c"
  log_changes$length_renamed_records <- 1
  expect_message(log_change_messages(log_changes), "Full update triggered")
  log_changes$renamed_records <- NULL
  log_changes$length_renamed_records <- 0
  log_changes$length_comment_records <- 1
  log_changes$length_deleted_records <- 1
  log_changes$length_updated_records <- 1
  log_changes$comment_records <- "comment_record"
  log_changes$deleted_records <- "deleted_record"
  log_changes$updated_records <- "updated_record"
  expect_message(log_change_messages(log_changes), "Deleted: deleted")
  expect_message(log_change_messages(log_changes), "Updated: updated")
  expect_message(log_change_messages(log_changes), "Comments: comment")
})
# sweep_dirs_for_cache ( Internal )
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
  withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
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
