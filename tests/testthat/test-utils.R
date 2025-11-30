# add_redcap_links_to_form ( Internal )
test_that("add_redcap_links_to_form works!", {
  project <- TEST_CLASSIC
  form <- data.frame(
    record_id = c("1", "2"),
    var_text = c("A", "B"),
    stringsAsFactors = FALSE
  )
  # Should add redcap_link column
  out <- add_redcap_links_to_form(form, project)
  expect_true("redcap_link" %in% colnames(out))
  expect_equal(length(out$redcap_link), 2)
  # Links should contain record_id and project id
  expect_all_true(grepl("id=1|id=2", out$redcap_link))
  project_id <- project$redcap$project_id
  compare <- paste0("_home\\.php\\?pid=",project_id,"&id=", form$record_id)
  expect_true(grepl(compare[1], out$redcap_link[1]))
  expect_true(grepl(compare[2], out$redcap_link[2]))
  # Should not modify other columns
  expect_equal(out$var_text, form$var_text)
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
  expect_equal(out$form1$record_id, "1")
  expect_equal(out$form2$record_id, "4")
  # Remove no records (NULL)
  out2 <- remove_from_form_list(form_list, id_col = "record_id", records = NULL)
  expect_equal(out2$form1$record_id, c("1", "2", "3"))
  expect_equal(out2$form2$record_id, c("2", "3", "4"))
  # Remove all records
  out3 <- remove_from_form_list(
    form_list, id_col = "record_id", records = c("1", "2", "3", "4"))
  expect_equal(nrow(out3$form1), 0)
  expect_equal(nrow(out3$form2), 0)
  # Empty form_list returns itself
  expect_equal(
    remove_from_form_list(list(), id_col = "record_id", records = c("1")),
    list())
})
# remove_records_from_project ( Internal )
test_that("remove_records_from_project works!", {
  project <- mock_project()
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
  expect_equal(project2$data$form_example$record_id, "1")
  expect_equal(project2$transformation$data$form_example$record_id, "1")
  # Remove no records (empty vector) should error
  expect_error(remove_records_from_project(project, records = character(0)))
  # Remove all records
  project3 <- remove_records_from_project(project, records = c("1", "2", "3"))
  expect_equal(nrow(project3$data$form_example), 0)
  expect_equal(nrow(project3$transformation$data$form_example), 0)
})
# split_choices ( Internal )
test_that("split_choices works!", {
  choices <- split_choices("1, one | 2, two")
  checkmate::expect_data_frame(choices, nrows = 2, ncols = 2)
  expect_false(anyNA(choices$code))
  expect_false(anyNA(choices$name))
  expect_true(choices$code[[1]]=="1")
  expect_true(choices$code[[2]]=="2")
  expect_true(choices$name[[1]]=="one")
  expect_true(choices$name[[2]]=="two")
  expect_error(split_choices("1. one | 2, two"))
  choices <- split_choices("1, one even with , second comma | 2, two")
  checkmate::expect_data_frame(choices, nrows = 2, ncols = 2)
  expect_false(anyNA(choices$code))
  expect_false(anyNA(choices$name))
})
test_that("split_choices works if space missing!", {
  choices <- split_choices("1,one | 2,two")
  checkmate::expect_data_frame(choices, nrows = 2, ncols = 2)
  expect_false(anyNA(choices$code))
  expect_false(anyNA(choices$name))
})
