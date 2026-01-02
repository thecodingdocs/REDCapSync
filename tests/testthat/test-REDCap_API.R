# get_redcap_metadata ( Internal )
test_that("get_redcap_metadata works!", {
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
  project <- mock_project()
  # Create a fake rcon with the methods used by rcon_result
  fake_rcon <- list(
    projectInformation = function() {
      list(project_id = "9999",
           project_title = "Fake Project",
           has_repeating_instruments_or_events = "0")
    },
    arms = function() data.frame(arm = character(0L),
                                 stringsAsFactors = FALSE),
    events = function() data.frame(event = character(0L),
                                   stringsAsFactors = FALSE),
    mapping = function() data.frame(),
    instruments = function() {
      data.frame(instrument = "form1", stringsAsFactors = FALSE)
    },
    repeatInstrumentEvent = function() data.frame(),
    metadata = function() data.frame(field_name = character(0L),
                                     stringsAsFactors = FALSE),
    users = function() data.frame(username = character(0L),
                                  stringsAsFactors = FALSE),
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
