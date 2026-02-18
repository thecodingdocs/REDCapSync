withr::local_envvar(REDCAPSYNC_CACHE = sanitize_path(withr::local_tempdir()))
# add_redcap_links_to_form ( Internal )
test_that("add_redcap_links_to_form works!", {
  project <- mock_test_project()$.internal
  form <- data.frame(
    record_id = c("1", "2"),
    var_text = c("A", "B"),
    stringsAsFactors = FALSE
  )
  # Should add redcap_link column
  out <- add_redcap_links_to_form(form, project)
  expect_true("redcap_link" %in% colnames(out))
  expect_length(out$redcap_link, 2L)
  # Links should contain record_id and project id
  expect_all_true(grepl("id=1|id=2", out$redcap_link))
  project_id <- project$redcap$project_id
  compare <- paste0("_home\\.php\\?pid=", project_id, "&id=", form$record_id)
  expect_true(grepl(compare[1L], out$redcap_link[1L]))
  expect_true(grepl(compare[2L], out$redcap_link[2L]))
  # Should not modify other columns
  expect_identical(out$var_text, form$var_text)
})
# remove_from_form_list ( Internal )
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
  out <- remove_from_form_list(
    form_list, id_col = "record_id", records = c("2", "3"))
  expect_identical(out$form1$record_id, "1")
  expect_identical(out$form2$record_id, "4")
  # Remove no records (NULL)
  out2 <- remove_from_form_list(form_list, id_col = "record_id", records = NULL)
  expect_identical(out2$form1$record_id, c("1", "2", "3"))
  expect_identical(out2$form2$record_id, c("2", "3", "4"))
  # Remove all records
  out3 <- remove_from_form_list(
    form_list, id_col = "record_id", records = c("1", "2", "3", "4"))
  expect_identical(nrow(out3$form1), 0L)
  expect_identical(nrow(out3$form2), 0L)
  # Empty form_list returns itself
  expect_identical(
    remove_from_form_list(list(), id_col = "record_id", records = "1"),
    list())
})
# remove_records_from_project ( Internal )
test_that("remove_records_from_project works!", {
  project <- load_test_project()$.internal
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
# split_choices ( Internal )
test_that("split_choices works!", {
  choices <- split_choices("1, one | 2, two")
  expect_data_frame(choices, nrows = 2L, ncols = 2L)
  expect_false(anyNA(choices$code))
  expect_false(anyNA(choices$name))
  expect_identical(choices$code[[1L]], "1")
  expect_identical(choices$code[[2L]], "2")
  expect_identical(choices$name[[1L]], "one")
  expect_identical(choices$name[[2L]], "two")
  expect_error(split_choices("1. one | 2, two"))
  choices <- split_choices("1, one even with , second comma | 2, two")
  expect_data_frame(choices, nrows = 2L, ncols = 2L)
  expect_false(anyNA(choices$code))
  expect_false(anyNA(choices$name))
})
test_that("split_choices works if space missing!", {
  choices <- split_choices("1,one | 2,two")
  expect_data_frame(choices, nrows = 2L, ncols = 2L)
  expect_false(anyNA(choices$code))
  expect_false(anyNA(choices$name))
})
test_that("split_choices works if split space missing!", {
  choices <- split_choices("1, one|2, two")
  expect_data_frame(choices, nrows = 2L, ncols = 2L)
  expect_false(anyNA(choices$code))
  expect_false(anyNA(choices$name))
})
