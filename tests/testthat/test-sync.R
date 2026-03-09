tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(REDCAPSYNC_CACHE_OVERRIDE = tempdir_file)
# sync (Exported)
test_that("sync works!", {
  tempdir_test <- sanitize_path(withr::local_tempdir())
  withr::local_envvar(REDCAPSYNC_CACHE_OVERRIDE = tempdir_test)
  local_mocked_bindings(
    sweep_dirs_for_cache = function(...) NULL,
    load_project = function(...) NULL,
    get_projects = function(...) .blank_project_details
  )
  expect_message(sync(), "No projects in cache")
  local_mocked_bindings(
    sweep_dirs_for_cache = function(...)
      NULL,
    get_projects = function(...) {
      data.frame(project_name = "TEST_CLASSIC", stringsAsFactors = FALSE)
    }
  )
  expect_message(sync(), "Unable to load")
  local_mocked_bindings(
    load_project = function(...) mock_test_project()
  )
  expect_message(sync(), "TEST projects do not communicate with the API")
})
# due_for_sync (Internal)
test_that("due_for_sync works!", {
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
# get_interim_log (Internal)
test_that("get_interim_log works!", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  interim_log <- head(project$redcap$log, n = 10L)
  # Update record 3
  project$redcap$has_log_access <- FALSE
  expect_message(get_interim_log(project), "You do not have logging access")
  project$redcap$has_log_access <- TRUE
  local_mocked_bindings(
    get_redcap_log = function(...) interim_log
  )
  expect_data_frame(get_interim_log(project), nrows = 0L)
  new_interim_log_info <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Create record (API) 99",
    details = "some details",
    record = "99",
    action_type = "Create",
    stringsAsFactors = FALSE
  )
  interim_log <- bind_rows(new_interim_log_info, interim_log)
  local_mocked_bindings(
    get_redcap_log = function(...) interim_log
  )
  extracted_interim <- get_interim_log(project)
  expect_data_frame(extracted_interim, nrows = 1L)
  expect_identical(new_interim_log_info, extracted_interim)
})
# remove_from_form_list (Internal)
test_that("remove_from_form_list works!", {
  # Setup: list of data.frames with record_id
  df1 <- data.frame(
    record_id = c("1", "2", "3"),
    value = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    record_id = c("2", "3", "4"),
    value = c("X", "Y", "Z"),
    stringsAsFactors = FALSE
  )
  form_list <- list(form1 = df1, form2 = df2)
  # Remove records "2" and "3"
  out <- remove_from_form_list(form_list = form_list,
                               id_col = "record_id",
                               records = c("2", "3"))
  expect_identical(out$form1$record_id, "1")
  expect_identical(out$form2$record_id, "4")
  # Remove no records (NULL)
  out2 <- remove_from_form_list(form_list, id_col = "record_id", records = NULL)
  expect_identical(out2$form1$record_id, c("1", "2", "3"))
  expect_identical(out2$form2$record_id, c("2", "3", "4"))
  # Remove all records
  out3 <- remove_from_form_list(form_list = form_list,
                                id_col = "record_id",
                                records = c("1", "2", "3", "4"))
  expect_identical(nrow(out3$form1), 0L)
  expect_identical(nrow(out3$form2), 0L)
  # Empty form_list returns itself
  expect_identical(remove_from_form_list(form_list = list(),
                                         id_col = "record_id",
                                         records = "1"),
                   list())
})
# remove_records_from_project (Internal)
test_that("remove_records_from_project works!", {
  project <- mock_test_project()$.internal
  # Add some records to form_example and transformation$data
  project$data$form_example <- data.frame(
    record_id = c("1", "2", "3"),
    var_text = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  project$metadata$id_col <- "record_id"
  project$transformation$data <- list(
    form_example = data.frame(
      record_id = c("1", "2", "3"),
      var_text = c("A", "B", "C"),
      stringsAsFactors = FALSE
    )
  )
  # Remove records "2" and "3"
  project2 <- remove_records_from_project(project, records = c("2", "3"))
  expect_identical(project2$data$form_example$record_id, "1")
  expect_identical(project2$transformation$data$form_example$record_id, "1")
  # Remove no records (empty vector) should error
  expect_error(remove_records_from_project(project, records = character(0L)))
  # Remove all records
  project3 <- remove_records_from_project(project, records = c("1", "2", "3"))
  expect_identical(nrow(project3$data$form_example), 0L)
  expect_identical(nrow(project3$transformation$data$form_example), 0L)
})
# sync_project (Internal)
test_that("sync_project will update if new", {
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
  expect_length(row_for_one, 1L)
  expect_length(row_for_two, 0L)
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
    summarize = TRUE,
    save_to_dir = TRUE,
    hard_reset = FALSE
  )
  row_for_one <- which(result$data$text$record_id == "1")
  row_for_two <- which(result$data$text$record_id == "2")
  updated_var_text_integer_one <- result$data$text$var_text_integer[row_for_one]
  updated_var_text_integer_two <- result$data$text$var_text_integer[row_for_two]
  expect_identical(new_var_text_integer, updated_var_text_integer_one)
  expect_identical(var_text_integer_two, updated_var_text_integer_two)
})
test_that("sync_project will work if not due", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  local_mocked_bindings(
    due_for_sync = function(...) FALSE
  )
  expect_message(sync_project(
    project = project,
    summarize = FALSE,
    save_to_dir = TRUE,
    hard_reset = FALSE
  ), "not due for sync")
})
# sync_project_check (Internal)
test_that("sync_project_check cannot connect!", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  project$internals$last_test_connection_outcome <- FALSE
  local_mocked_bindings(
    test_project_token = function(...) project
  )
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
  local_mocked_bindings(
    test_project_token = function(...) project,
    get_interim_log = function(...) interim_log
  )
  # Mock the test_project_token to simulate successful connection
  # Call sync_project_check with hard_reset = FALSE
  result <- sync_project_check(project, hard_reset = FALSE)
  expect_false(result$internals$was_updated)
})
# sync_project_hard_reset (Internal)
test_that("sync_project_hard_reset works!", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  local_mocked_bindings(
    get_redcap_metadata = function(...) project,
    update_project_links = function(...) project,
    get_redcap_data = function(...) project$data,
    get_redcap_log = function(...) project$redcap$log
  )
  result <- project
  result$metadata <- .blank_project$metadata
  result["data"] <- list(NULL)
  result <- sync_project_hard_reset(result)
  expect_identical(result$metadata, project$metadata)
  expect_identical(result$data, project$data)
})
# sync_project_refresh (Internal)
test_that("sync_project_refresh works!", {
})
