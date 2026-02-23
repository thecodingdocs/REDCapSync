withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
test_that("R6 object works!", {
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
test_that("active_bindings R6 read-only", {
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
test_that("R6 info works!", {
  project_r6 <- mock_test_project()
  expect_message(project_r6$info(), "Project Name: TEST_CLASSIC")
  expect_message(project_r6$info(), "Token Name: REDCAPSYNC_TEST_CLASSIC")
  expect_message(project_r6$info(), "PID: 12341")
})
test_that("R6 add_summary and remove_summaries works!", {
  temp_dir <- assert_directory(Sys.getenv("REDCAPSYNC_CACHE"))
  project_r6 <- mock_test_project()
  summary_names <- project_r6$.internal$summary |>
    names() |>
    setdiff("all_records")
  expect_false("new_summary" %in% summary_names)
  project_r6$add_summary(
    "new_summary",
    filter_field = "var_yesno",
    filter_choices = "Yes",
    dir_other = temp_dir
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
  expect_length(summary_names, 0)
})
test_that("R6 save works!", {
  withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
  project <- mock_test_project()$.internal
  dir_path <- project$dir_path
  proj_path <- file.path(dir_path, "R_objects", "TEST_CLASSIC_REDCapSync.RData")
  project_r6 <- REDCapSync_project$new(project)
  expect_false(test_file_exists(proj_path))
  expect_message(project_r6$save(), "TEST projects do not save to directories")
  expect_false(test_file_exists(proj_path))
  project$internals$is_test <- FALSE
  project_r6 <- REDCapSync_project$new(project)
  project_r6$save()
  test_file_exists(proj_path)
})
test_that("R6 save works!", {
  withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
  project <- mock_test_project()$.internal
  dir_path <- project$dir_path
  proj_path <- file.path(dir_path, "R_objects", "TEST_CLASSIC_REDCapSync.RData")
  project_r6 <- REDCapSync_project$new(project)
  expect_message(project_r6$summarize(), "TEST projects do not")
  expect_message(project_r6$sync(), "TEST projects do not")
})
