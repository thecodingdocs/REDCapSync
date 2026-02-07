test_that("R6 object works!", {
  project_r6 <- REDCapSync_project$new(load_test_project()$.internal)
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
  project_r6 <- REDCapSync_project$new(load_test_project()$.internal)
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
  project_r6 <- REDCapSync_project$new(load_test_project()$.internal)
  expect_message(project_r6$info(), "Project Name: TEST_CLASSIC")
  expect_message(project_r6$info(), "Token Name: REDCapSync_TEST_CLASSIC")
  expect_message(project_r6$info(), "PID: 12341")
})
test_that("R6 add_summary works!", {
  temp_dir <- withr::local_tempdir() |> sanitize_path()
  project_r6 <- REDCapSync_project$new(load_test_project()$.internal)
  project_r6$add_summary(
    "new_summary",
    filter_field = "var_yesno",
    filter_choices = "Yes",
    dir_other = temp_dir
  )
  x <- project_r6$generate_summary("new_summary")
  expect_list(x)
  project_r6$save_summary("new_summary")
})
