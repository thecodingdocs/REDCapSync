# get_redcap_metadata ( Internal )
test_that("get_redcap_metadata works on real server, simple!", {
  skip_on_cran()
  project <- REDCapR_project()$.internal
  # expect_false(project$internals$ever_connected)
  expect_data_frame(as.data.frame(project$metadata$forms) , nrows = 0)
  expect_data_frame(as.data.frame(project$metadata$fields) , nrows = 0)
  expect_data_frame(as.data.frame(project$metadata$choices) , nrows = 0)
  expect_data_frame(as.data.frame(project$redcap$users) , nrows = 0)
  expect_data_frame(as.data.frame(project$redcap$project_info) , nrows = 0)
  project_with_metadata <- withr::with_envvar(
    c(REDCapSync_TEST_REDCAPR = "9A068C425B1341D69E83064A2D273A70"),
    {get_redcap_metadata(project)})
  # expect_true(project_with_metadata$internals$ever_connected)
  expect_data_frame(project_with_metadata$metadata$forms, min.rows = 1)
  expect_data_frame(project_with_metadata$metadata$fields, min.rows = 1)
  expect_data_frame(project_with_metadata$metadata$choices, min.rows = 1)
  expect_data_frame(project_with_metadata$redcap$users, min.rows = 1)
  expect_data_frame(project_with_metadata$redcap$project_info , nrows = 1)
})
test_that("get_redcap_metadata works on real server, longitudinal!", {
  skip_on_cran()
  project <- REDCapR_project()$.internal
  expect_data_frame(as.data.frame(project$metadata$forms) , nrows = 0)
  expect_data_frame(as.data.frame(project$metadata$fields) , nrows = 0)
  expect_data_frame(as.data.frame(project$metadata$choices) , nrows = 0)
  expect_data_frame(as.data.frame(project$redcap$users) , nrows = 0)
  expect_data_frame(as.data.frame(project$redcap$project_info) , nrows = 0)
  project_with_metadata <- withr::with_envvar(
    c(REDCapSync_TEST_REDCAPR = "DA6F2BB23146BD5A7EA3408C1A44A556"),
    {get_redcap_metadata(project)})
  expect_data_frame(project_with_metadata$metadata$forms, min.rows = 1)
  expect_data_frame(project_with_metadata$metadata$fields, min.rows = 1)
  expect_data_frame(project_with_metadata$metadata$choices, min.rows = 1)
  expect_data_frame(project_with_metadata$redcap$users, min.rows = 1)
  expect_data_frame(project_with_metadata$redcap$project_info , nrows = 1)
})
test_that("get_redcap_metadata works with fixture data (simple)", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  # Mock rcon_result to return fixture data
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  expect_list(result)
  expect_identical(result$project_name, project$project_name)
  expect_true(!is.null(result$metadata))
  expect_true(!is.null(result$redcap$project_info))
  expect_data_frame(result$metadata$forms, min.rows = 1)
  expect_data_frame(result$metadata$fields, min.rows = 1)
})
test_that("get_redcap_metadata works with fixture data (longitudinal)", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  project$redcap$is_longitudinal <- TRUE
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_LONGITUDINAL_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  expect_list(result)
  expect_true(result$metadata$is_longitudinal)
  expect_true(result$metadata$has_arms)
  expect_data_frame(result$metadata$arms, min.rows = 1)
  expect_data_frame(result$metadata$events, min.rows = 1)
})
test_that("get_redcap_metadata works with fixture data (repeating forms)", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REPEATING_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  expect_list(result)
  expect_true(result$metadata$has_repeating_forms_or_events)
})
test_that("get_redcap_metadata extracts correct project info from fixture", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  fixture <- readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {fixture})
  result <- get_redcap_metadata(project)
  # Verify project info fields are populated
  expect_true(!is.null(result$redcap$project_title))
  expect_true(!is.null(result$redcap$project_id))
  expect_identical(result$redcap$project_info, fixture$project_info)
})
test_that("get_redcap_metadata handles forms from fixture", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  expect_data_frame(result$metadata$forms)
  expect_true("form_name" %in% colnames(result$metadata$forms))
})
test_that("get_redcap_metadata handles fields from fixture", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  expect_data_frame(result$metadata$fields)
  expect_true("field_name" %in% colnames(result$metadata$fields))
  expect_true("form_name" %in% colnames(result$metadata$fields))
})
test_that("get_redcap_metadata handles choices from fixture", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  expect_data_frame(result$metadata$choices)
})
test_that("get_redcap_metadata handles users from fixture", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  expect_data_frame(result$redcap$users)
  expect_true(result$redcap$has_user_access)
})
test_that("get_redcap_metadata detects logging access from fixture", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  # Check if logging access is correctly detected
  expect_true(is.logical(result$redcap$has_log_access))
})
test_that("get_redcap_metadata handles missing_data_codes from fixture", {
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
  project <- load_test_project()$.internal
  project$dir_path <- set_dir(test_dir)
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  # Check missing codes handling
  if (!is.na(result$metadata$missing_codes)) {
    expect_data_frame(result$metadata$missing_codes)
  }
})
test_that("get_redcap_metadata preserves project name", {
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
  original_name <- "TEST_CLASSIC"
  project <- load_test_project()$.internal
  project$project_name <- original_name
  project$dir_path <- set_dir(test_dir)
  mockery::stub(get_redcap_metadata, "rcon_result", function(project) {
    readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
  })
  result <- get_redcap_metadata(project)
  expect_identical(result$project_name, original_name)
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
test_that("get_redcap_log works!", {
})
# get_redcap_denormalized ( Internal )
test_that("get_redcap_denormalized works!", {
  skip_on_cran()
  project <- REDCapR_project()$.internal
  expect_data_frame(as.data.frame(project$data) , nrows = 0)
  suppressWarnings({ #REDCapR has warnings
    project_data <- withr::with_envvar(
      c(REDCapSync_TEST_REDCAPR = "DA6F2BB23146BD5A7EA3408C1A44A556"),
      {get_redcap_denormalized(project)})
  })
  expect_data_frame(project_data, min.rows = 1)
})
test_that("get_redcap_denormalized works no API call!", {
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
  original_name <- "TEST_CLASSIC"
  project <- load_test_project()$.internal
  project$project_name <- original_name
  project$dir_path <- set_dir(test_dir)
  project <- REDCapR_project()$.internal
  mockery::stub(
    get_redcap_denormalized,
    "REDCapR::redcap_read",
    function(...) {
      readRDS(test_path("fixtures", "TEST_REDCAPR_SIMPLE_call_list.rds"))
    }
  )
  # denorm <- get_redcap_denormalized(project)
  # normalize_redcap(denorm, project)
})
# get_redcap_report ( Exported )
test_that("get_redcap_report works!", {
})
# get_redcap_data ( Internal )
test_that("get_redcap_data works!", {
})
# rename_forms_redcap_to_default ( Internal )
test_that("rename_forms_redcap_to_default works!", {
})
test_that("rcon_result returns expected structure without real API calls", {
  project <- load_test_project()$.internal
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
test_that("upload_form_to_redcap() calls REDCapR::redcap_write expected args", {
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
  mockery::stub(upload_form_to_redcap, "REDCapR::redcap_write", function(...) {
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
