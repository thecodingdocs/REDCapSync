withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
# get_redcap_metadata ( Internal )
test_that("get_redcap_metadata works on real server, simple!", {
  skip_on_cran()
  project_name <- "TEST_REDCAPR_SIMPLE"
  project <- real_test_project(project_name)$.internal
  # expect_false(project$internals$ever_connected)
  expect_data_frame(as.data.frame(project$metadata$forms), nrows = 0)
  expect_data_frame(as.data.frame(project$metadata$fields), nrows = 0)
  expect_data_frame(as.data.frame(project$metadata$choices), nrows = 0)
  expect_data_frame(as.data.frame(project$redcap$users), nrows = 0)
  expect_data_frame(as.data.frame(project$redcap$project_info), nrows = 0)
  project_with_metadata <- withr::with_envvar(real_dev_tokens, {
    get_redcap_metadata(project)
  })
  # expect_true(project_with_metadata$internals$ever_connected)
  expect_data_frame(project_with_metadata$metadata$forms, min.rows = 1)
  expect_data_frame(project_with_metadata$metadata$fields, min.rows = 1)
  expect_data_frame(project_with_metadata$metadata$choices, min.rows = 1)
  expect_data_frame(project_with_metadata$redcap$users, min.rows = 1)
  expect_data_frame(project_with_metadata$redcap$project_info, nrows = 1)
})
test_that("get_redcap_metadata works on real server, longitudinal!", {
  skip_on_cran()
  project_name <- "TEST_REDCAPR_SIMPLE"
  project <- real_test_project(project_name)$.internal
  expect_data_frame(as.data.frame(project$metadata$forms), nrows = 0)
  expect_data_frame(as.data.frame(project$metadata$fields), nrows = 0)
  expect_data_frame(as.data.frame(project$metadata$choices), nrows = 0)
  expect_data_frame(as.data.frame(project$redcap$users), nrows = 0)
  expect_data_frame(as.data.frame(project$redcap$project_info), nrows = 0)
  project_with_metadata <- withr::with_envvar(real_dev_tokens, {
    get_redcap_metadata(project)
  })
  expect_data_frame(project_with_metadata$metadata$forms, min.rows = 1)
  expect_data_frame(project_with_metadata$metadata$fields, min.rows = 1)
  expect_data_frame(project_with_metadata$metadata$choices, min.rows = 1)
  expect_data_frame(project_with_metadata$redcap$users, min.rows = 1)
  expect_data_frame(project_with_metadata$redcap$project_info, nrows = 1)
})
test_that("get_redcap_metadata works with fixture data (classic)", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  mockery::stub(get_redcap_metadata, "rcon_result", call_list)
  result <- get_redcap_metadata(project)
  expect_identical(result$project_name, project$project_name)
  expect_identical(call_list$project_info, project$redcap$project_info)
  project$metadata <- .blank_project$metadata # clear exisiting data
  expect_null(project$metadata$fields)
  expect_null(project$metadata$forms)
  expect_list(result)
  expect_true(!is.null(result$metadata))
  expect_true(!is.null(result$redcap$project_info))
  expect_data_frame(result$metadata$forms, min.rows = 1)
  expect_data_frame(result$metadata$fields, min.rows = 1)
  expect_true(!is.null(result$redcap$project_title))
  expect_true(!is.null(result$redcap$project_id))
  expect_data_frame(result$metadata$forms)
  expect_true("form_name" %in% colnames(result$metadata$forms))
  expect_data_frame(result$metadata$fields)
  expect_true("field_name" %in% colnames(result$metadata$fields))
  expect_true("form_name" %in% colnames(result$metadata$fields))
  expect_data_frame(result$metadata$choices)
  expect_data_frame(result$redcap$users)
  expect_true(result$redcap$has_user_access)
  expect_true(is.logical(result$redcap$has_log_access))
  if (is_something(result$metadata$missing_codes)) {
    expect_data_frame(result$metadata$missing_codes)
  }
  expect_identical(result$project_name, project_name)
})
test_that("get_redcap_metadata works with fixture data (longitudinal)", {
  project_name <- "TEST_REDCAPR_LONGITUDINAL"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  mockery::stub(get_redcap_metadata, "rcon_result", call_list)
  project$metadata <- .blank_project$metadata # clear exisiting data
  expect_null(project$metadata$fields)
  expect_null(project$metadata$forms)
  result <- get_redcap_metadata(project)
  expect_list(result)
  expect_true(result$metadata$is_longitudinal)
  expect_true(result$metadata$has_arms)
  expect_data_frame(result$metadata$arms, min.rows = 1)
  expect_data_frame(result$metadata$events, min.rows = 1)
})
test_that("get_redcap_metadata works with fixture data (repeating forms)", {
  project_name <- "TEST_REPEATING"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  mockery::stub(get_redcap_metadata, "rcon_result", call_list)
  project$metadata <- .blank_project$metadata # clear exisiting data
  result <- get_redcap_metadata(project)
  expect_list(result)
  expect_true(result$metadata$has_repeating_forms_or_events)
})
# add_field_elements ( Internal )
test_that("add_field_elements works!", {
})
# update_project_links ( Internal )
test_that("update_project_links works!", {
})
# get_redcap_files ( Internal )
test_that("get_redcap_files works!", {
})
# get_redcap_users ( Internal )
test_that("get_redcap_users works!", {
})
# get_redcap_log ( Internal )
#need dev server with log access?
test_that("get_redcap_log works on fixture!", {
  project_name <- "TEST_REPEATING"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  mockery::stub(get_redcap_log, "exportLogging", call_list$logging)
  result <- get_redcap_log(project)
  expect_data_frame(result, min.rows = 1)
})
# get_redcap_denormalized ( Internal )
test_that("get_redcap_denormalized works!", {
  skip_on_cran()
  project <- real_test_project("TEST_REDCAPR_LONGITUDINAL")$.internal
  expect_data_frame(as.data.frame(project$data), nrows = 0)
  suppressWarnings({ #REDCapR has warnings
    project_data <- withr::with_envvar(
      real_dev_tokens,
      {get_redcap_denormalized(project)})
  })
  expect_data_frame(project_data, min.rows = 1)
})
test_that("get_redcap_denormalized works no API call!", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  call_list <- mock_test_calls(project_name)
  mockery::stub(get_redcap_denormalized, "redcap_read", call_list)
  result <- get_redcap_denormalized(project)
  expect_data_frame(result, min.rows = 1)
})
# get_redcap_report ( Exported )
test_that("get_redcap_report works!", {
})
# get_redcap_data ( Internal )
test_that("get_redcap_data works!", {
  # project_name <- "TEST_CLASSIC"
  # temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  # project <- mock_test_project(project_name)$.internal
  # call_list <- mock_test_calls(project_name)
  # mockery::stub(get_redcap_denormalized, "redcap_read", call_list)
  # result <- get_redcap_denormalized(project)
  # expect_data_frame(result, min.rows = 1)
})
# rename_forms_redcap_to_default ( Internal )
test_that("rename_forms_redcap_to_default works!", {
})
test_that("rcon_result returns expected structure without real API calls", {
  project <- mock_test_project()$.internal
  # Create a fake rcon with the methods used by rcon_result
  fake_rcon <- list(
    projectInformation = function() {
      list(project_id = "9999",
           project_title = "Fake Project",
           has_repeating_instruments_or_events = "0")
    },
    arms = function() {
      data.frame(arm = character(0L), stringsAsFactors = FALSE)
    },
    events = function() {
      data.frame(event = character(0L), stringsAsFactors = FALSE)
    },
    mapping = function() data.frame(),
    instruments = function() {
      data.frame(instrument = "form1", stringsAsFactors = FALSE)
    },
    repeatInstrumentEvent = function() data.frame(),
    metadata = function() {
      data.frame(field_name = character(0L), stringsAsFactors = FALSE)
    },
    users = function() {
      data.frame(username = character(0L), stringsAsFactors = FALSE)
    },
    user_roles = function() data.frame(),
    user_role_assignment = function() data.frame(),
    dags = function() data.frame(),
    dag_assignment = function() data.frame(),
    fileRepository = function() data.frame()
  )
  # Stub redcapConnection and exportLogging inside rcon_result to avoid API call
  mockery::stub(rcon_result, "redcapConnection", function(url, token) fake_rcon)
  mockery::stub(rcon_result, "exportLogging", function(rcon, beginTime) {
    data.frame()})
  out <- rcon_result(project)
  # replace with real data from fixtures
  expect_type(out, "list")
  # core elements present
  expect_true("project_info" %in% names(out))
  expect_true("arms" %in% names(out))
  expect_true("events" %in% names(out))
  expect_true("mapping" %in% names(out))
  expect_true("forms" %in% names(out))
  expect_true("repeating" %in% names(out))
  expect_true("fields" %in% names(out))
  expect_true("logging" %in% names(out))
  expect_true("users" %in% names(out))
  expect_true("user_roles" %in% names(out))
  expect_true("user_role_assignment" %in% names(out))
  expect_true("dags" %in% names(out))
  expect_true("dag_assignment" %in% names(out))
  expect_true("file_repository" %in% names(out))
  # check a few returned values come from our fake rcon
  expect_identical(out$project_info$project_id, "9999")
  expect_s3_class(out$forms, "data.frame")
})
test_that("upload_form_to_redcap() calls redcap_write expected args", {
  project <- list(
    links = list(redcap_uri = "https://redcap.fake.edu/api/")
  )
  to_be_uploaded <- data.frame(
    record_id = c("1", "2"),
    age = c(50L, 60L),
    stringsAsFactors = FALSE
  )
  captured <- list()
  # Stub token lookup so we don't depend on env vars
  mockery::stub(upload_form_to_redcap, "get_project_token", function(project) {
    "0123456789ABCDEF0123456789ABCDEF"
  })
  # Stub the network call and capture args
  mockery::stub(upload_form_to_redcap, "redcap_write", function(...) {
    captured <<- list(...)
    list(success = TRUE)
  })
  result <- upload_form_to_redcap(
    to_be_uploaded = to_be_uploaded,
    project = project,
    batch_size = 123L
  )
  expect_true(isTRUE(result$success))
  # Check behavior of RosyUtils::all_character_cols()
  expect_true(is.data.frame(captured$ds_to_write))
  expect_true(all(vapply(captured$ds_to_write, is.character, logical(1L))))
  expect_identical(captured$batch_size, 123L)
  expect_identical(captured$interbatch_delay, 0.2)
  expect_false(captured$continue_on_error)
  expect_identical(captured$redcap_uri, "https://redcap.fake.edu/api/")
  expect_identical(captured$token, "0123456789ABCDEF0123456789ABCDEF")
  expect_true(captured$overwrite_with_blanks)
})
