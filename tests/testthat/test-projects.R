##test-get_projects
test_that("get_projects is df and has appropriate columns",{
  df <- get_projects()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
  df <- blank_project()
  expect_s3_class(df,"data.frame")
  expect_true(all(colnames(df)%in%internal_blank_project_cols))
})
test_that("check_folder_for_projects works",{
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  test_dir_folder <- file.path(test_dir, "R_objects")
  test_file1 <- file.path(test_dir, "R_objects","not_a_proj.RData")
  test_file2 <- file.path(test_dir, "R_objects","PROJ_REDCapSync.RData")
  test_file3 <- file.path(test_dir,"ANOTHER_PROJ_REDCapSync.RData")
  test_file4 <- file.path(test_dir,"ANOTHER_PROJ_wrong_suffix.RData")
  dir.create(test_dir_folder)
  file.create(test_file1)
  file.create(test_file2)
  file.create(test_file3)
  file.create(test_file4)
  local_mocked_bindings(
    assert_dir = function(...) test_dir
  )
  #check without validation
  expect_false(test_file1%in%check_folder_for_projects(file_path = test_dir,validate = FALSE))
  expect_contains(check_folder_for_projects(file_path = test_dir,validate = FALSE),test_file2)
  expect_contains(check_folder_for_projects(file_path = test_dir,validate = FALSE),test_file3)
  expect_false(test_file4%in%check_folder_for_projects(file_path = test_dir,validate = FALSE))
  #check validation
  expect_false(test_file1%in%check_folder_for_projects(file_path = test_dir,validate = TRUE))
  expect_contains(check_folder_for_projects(file_path = test_dir,validate = TRUE),test_file2)
  expect_false(test_file3%in%check_folder_for_projects(file_path = test_dir,validate = TRUE))
  expect_false(test_file4%in%check_folder_for_projects(file_path = test_dir,validate = TRUE))
})
