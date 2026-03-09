tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar("REDCAPSYNC_CACHE_OVERRIDE" = tempdir_file)
test_that("REDCapSync_project object works!", {
  project_r6 <- REDCapSync_project$new(mock_test_project()$.internal)
  expect_r6_class(project_r6, "REDCapSync_project")
  REDCapSync_project$public_methods |> names()
  REDCapSync_project$active |> names()
  REDCapSync_project$private_fields |> names()
  expect_r6(
    project_r6,
    classes = "REDCapSync_project",
    private = names(REDCapSync_project$private_fields),
    public = c(names(REDCapSync_project$public_methods)),
    cloneable = FALSE
  )
})
test_that("REDCapSync_project active_bindings are read-only", {
  project_r6 <- mock_test_project()
  names(REDCapSync_project$active)
  for (active_name in names(REDCapSync_project$active)){
    original <- project_r6[[active_name]]
    expect_in(active_name, names(REDCapSync_project$active))
    expect_message({
      project_r6[[active_name]] <- "fake data"
    }, "is read only")
    expect_identical(original, project_r6[[active_name]])
  }
})
test_that("REDCapSync_project with test projects!", {
  tempdir_test <- sanitize_path(withr::local_tempdir())
  withr::local_envvar(REDCAPSYNC_CACHE_OVERRIDE = tempdir_test)
  project <- mock_test_project()$.internal
  dir_path <- project$dir_path
  proj_path <- file.path(dir_path, "R_objects", "TEST_CLASSIC_REDCapSync.RData")
  project_r6 <- REDCapSync_project$new(project)
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
  project_r6 <- REDCapSync_project$new(project)
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
# REDCapSync_project$add_field (Exported)
# REDCapSync_project$remove_fields (Exported)
test_that("REDCapSync_project$add_field and remove_fields works!", {
  project_r6 <- mock_test_project()
  expect_message(project_r6$add_field(), "placeholder")
  expect_message(project_r6$remove_fields(), "placeholder")
})
# REDCapSync_project$add_summary (Exported)
# REDCapSync_project$remove_summaries (Exported)
test_that("REDCapSync_project$add_summary and remove_summaries works!", {
  tempdir_test <- sanitize_path(withr::local_tempdir())
  withr::local_envvar(REDCAPSYNC_CACHE_OVERRIDE = tempdir_test)
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
# REDCapSync_project$generate_summary (Exported)
test_that("REDCapSync_project$generate_summary works!", {
})
# REDCapSync_project$info (Exported)
test_that("REDCapSync_project$info works!", {
  project_r6 <- mock_test_project()
  expect_message(project_r6$info(), "Project Name: TEST_CLASSIC")
  expect_message(project_r6$info(), "Token Name: REDCAPSYNC_TEST_CLASSIC")
  expect_message(project_r6$info(), "PID: 12341")
})
# REDCapSync_project$initialize (Exported)
test_that("REDCapSync_project$initialize works!", {
})
test_that("REDCapSync_project$remove_fields works!", {
})
test_that("REDCapSync_project$remove_summaries works!", {
})
# REDCapSync_project$save (Exported)
test_that("REDCapSync_project$save works!", {
})
# REDCapSync_project$save_summary (Exported)
test_that("REDCapSync_project$save_summary works!", {
})
# REDCapSync_project$summarize (Exported)
test_that("REDCapSync_project$summarize works!", {
})
# REDCapSync_project$sync (Exported)
test_that("REDCapSync_project$sync works!", {
})
# REDCapSync_project$test_token (Exported)
test_that("REDCapSync_project$test_token works!", {
})
# REDCapSync_project$upload (Exported)
test_that("REDCapSync_project$upload works!", {
  project <- mock_test_project()$.internal
  project$internals$is_test <- FALSE # override test block for tests
  project_r6 <- REDCapSync_project$new(project)
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
  to_be_uploaded <- project_r6$data$text |> utils::head(2L)
  to_be_uploaded$var_text_only <- c("New text", "Add text")
  to_be_uploaded$text_complete <- "Complete"
  expect_no_error(project_r6$upload(to_be_uploaded))
})
# REDCapSync_project$url_launch (Exported)
test_that("REDCapSync_project$url_launch works!", {
})
