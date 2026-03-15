tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
test_that("REDCapSyncProject object works!", {
  project_r6 <- REDCapSyncProject$new(mock_test_project()$.internal)
  expect_r6_class(project_r6, "REDCapSyncProject")
  expect_r6(
    project_r6,
    classes = "REDCapSyncProject",
    private = names(REDCapSyncProject$private_fields),
    public = c(names(REDCapSyncProject$public_methods)),
    cloneable = FALSE
  )
})
test_that("REDCapSyncProject active_bindings are read-only", {
  project_r6 <- mock_test_project()
  names(REDCapSyncProject$active)
  for (active_name in names(REDCapSyncProject$active)){
    original <- project_r6[[active_name]]
    expect_in(active_name, names(REDCapSyncProject$active))
    expect_message({
      project_r6[[active_name]] <- "fake data"
    }, "is read only")
    expect_identical(original, project_r6[[active_name]])
  }
})
test_that("REDCapSyncProject with test projects!", {
  tempdir_test <- sanitize_path(withr::local_tempdir())
  withr::local_envvar(R_USER_CACHE_DIR = tempdir_test)
  project <- mock_test_project()$.internal
  dir_path <- project$dir_path
  proj_path <- file.path(dir_path, "R_objects", "TEST_CLASSIC_REDCapSync.RData")
  project_r6 <- REDCapSyncProject$new(project)
  expect_message(project_r6$test_token(), "TEST projects do not")
  expect_message(project_r6$url_launch(), "TEST projects do not")
  expect_message(project_r6$summarize(), "TEST projects do not")
  expect_message(project_r6$sync(), "TEST projects do not")
  expect_message(project_r6$save_summary(), "TEST projects do not")
  expect_message(project_r6$upload(), "TEST projects do not")
  expect_false(test_file_exists(proj_path))
  expect_message(project_r6$save(), "TEST projects do not save to directories")
  expect_false(test_file_exists(proj_path))
  set_dir(project$dir_path)
  project$internals$is_test <- FALSE
  project_r6 <- REDCapSyncProject$new(project)
  project_r6$save()
  expect_file_exists(proj_path)
  project_r6$remove_summaries()
  project_r6$add_summary(
    summary_name = "fake"
  )
  project_r6$summarize()
  expect_file_exists(file.path(dir_path, "output", "TEST_CLASSIC_fake.xlsx"))
  project_r6$add_summary(
    summary_name = "fake2"
  )
  project_r6$save_summary(
    summary_name = "fake2"
  )
  expect_file_exists(file.path(dir_path, "output", "TEST_CLASSIC_fake2.xlsx"))
  expected_link <- "https://redcap.fake.edu/redcap_v16.1.1/index.php?pid=12341"
  expect_identical(project_r6$url_launch(open_browser = FALSE), expected_link)
})
# REDCapSyncProject$add_field (Exported)
# REDCapSyncProject$remove_fields (Exported)
test_that("REDCapSyncProject$add_field and remove_fields works!", {
  project_r6 <- mock_test_project()
  expect_message(project_r6$add_field(), "placeholder")
  expect_message(project_r6$remove_fields(), "placeholder")
})
# REDCapSyncProject$add_summary (Exported)
# REDCapSyncProject$remove_summaries (Exported)
test_that("REDCapSyncProject$add_summary and remove_summaries works!", {
  tempdir_test <- sanitize_path(withr::local_tempdir())
  withr::local_envvar(R_USER_CACHE_DIR = tempdir_test)
  project_r6 <- mock_test_project()
  summary_names <- project_r6$.internal$summary |>
    names() |>
    setdiff("all_records")
  expect_false("new_summary" %in% summary_names)
  project_r6$add_summary(
    "new_summary",
    filter_field = "var_yesno",
    filter_choices = "Yes",
    dir_other = tempdir_test
  )
  summary_names <- project_r6$.internal$summary |>
    names() |>
    setdiff("all_records")
  expect_in("new_summary", summary_names)
  x <- project_r6$generate_summary("new_summary")
  expect_list(x)
  project_r6$remove_summaries("REDCapSync")
  summary_names <- project_r6$.internal$summary |>
    names() |>
    setdiff("all_records")
  expect_false("REDCapSync" %in% summary_names)
  expect_in("REDCapSync_raw", summary_names)
  expect_message(project_r6$remove_summaries(), "Cleared project summaries")
  summary_names <- project_r6$.internal$summary |>
    names() |>
    setdiff("all_records")
  expect_length(summary_names, 0L)
  x <- project_r6$generate_summary(filter_field = "var_branching",
                                   filter_choices = "Yes")
  expect_all_true(x$merged$var_branching == "Yes")
})
# REDCapSyncProject$generate_summary (Exported)
test_that("REDCapSyncProject$generate_summary works!", {
})
# REDCapSyncProject$info (Exported)
test_that("REDCapSyncProject$info works!", {
  project_r6 <- mock_test_project()
  expect_message(project_r6$info(), "Project Name: TEST_CLASSIC")
  expect_message(project_r6$info(), "Token Name: REDCAPSYNC_TEST_CLASSIC")
  expect_message(project_r6$info(), "PID: 12341")
})
# REDCapSyncProject$initialize (Exported)
test_that("REDCapSyncProject$initialize works!", {
})
test_that("REDCapSyncProject$remove_fields works!", {
})
test_that("REDCapSyncProject$remove_summaries works!", {
})
# REDCapSyncProject$save (Exported)
test_that("REDCapSyncProject$save works!", {
})
# REDCapSyncProject$save_summary (Exported)
test_that("REDCapSyncProject$save_summary works!", {
})
# REDCapSyncProject$summarize (Exported)
test_that("REDCapSyncProject$summarize works!", {
})
# REDCapSyncProject$sync (Exported)
test_that("REDCapSyncProject$sync works!", {
})
# REDCapSyncProject$test_token (Exported)
test_that("REDCapSyncProject$test_token works!", {
})
# REDCapSyncProject$upload (Exported)
test_that("REDCapSyncProject$upload works!", {
  project <- mock_test_project()$.internal
  project$internals$is_test <- FALSE # override test block for tests
  project_r6 <- REDCapSyncProject$new(project)
  local_mocked_bindings(
    upload_form_to_redcap = function(...) message("Would have uploaded!"),
    sync_project_refresh = function(...) {
      message("Would have synced!")
      project
    }
  )
  local_mocked_bindings(
    Sys.sleep = function(...) {
      NULL
    },
    .package = "base"
  )
  expect_error(project_r6$upload(mtcars), "names in your form must match")
  to_be_uploaded <- head(project_r6$data$text, 2L)
  to_be_uploaded$var_text_only <- c("New text", "Add text")
  to_be_uploaded$text_complete <- "Complete"
  expect_no_error(project_r6$upload(to_be_uploaded))
})
# REDCapSyncProject$url_launch (Exported)
test_that("REDCapSyncProject$url_launch works!", {
})
