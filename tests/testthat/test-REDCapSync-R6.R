test_that("R6 object works!", {
  project_r6 <- REDCapSync_project$new(mock_project())
  expect_r6_class(project_r6,"REDCapSync_project")
  REDCapSync_project$public_methods |> names()
  REDCapSync_project$active |> names()
  REDCapSync_project$private_fields |> names()
  checkmate::expect_r6(
    project_r6,
    classes = "REDCapSync_project",
    private = names(REDCapSync_project$private_fields),
    public = c(names(REDCapSync_project$public_methods)),
    cloneable = FALSE
  )
})
test_that("active_bindings R6 read-only", {
  project_r6 <- REDCapSync_project$new(mock_project())
  names(REDCapSync_project$active)
  for (active_name in names(REDCapSync_project$active)){
    original <- project_r6[[active_name]]
    expect_in(active_name,names(REDCapSync_project$active))
    expect_message({project_r6[[active_name]] <- "fake data"},"is read only")
    expect_identical(original,project_r6[[active_name]])
  }
})
