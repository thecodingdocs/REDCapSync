# upload_form_to_REDCap ( Exported )
test_that("upload() uploads a single data.frame and then hard-check syncs", {
  # Load test project list and create R6 wrapper
  proj_list <- REDCapSync:::TEST_CLASSIC
  proj_list$internals$labelled <- FALSE
  project <- REDCapSync_project$new(proj_list)
  upload_calls <- 0L
  sync_calls <- 0L
  df <- data.frame(record_id = c("1", "2"), age = c(50, 60))
  testthat::local_mocked_bindings(
    upload_form_to_redcap = function(to_be_uploaded, project, batch_size) {
      upload_calls <<- upload_calls + 1L
      expect_identical(to_be_uploaded, df)   # no conversion expected
      expect_true(is.list(project))
      expect_identical(batch_size, 123L)
      invisible(TRUE)
    },
    sync_project = function(project, hard_check, ...) {
      sync_calls <<- sync_calls + 1L
      expect_true(isTRUE(hard_check))
      project
    }
  )
  # Check `upload()` returns invisible(TRUE)
  res <- withVisible(project$upload(df, batch_size = 123L))
  expect_true(res$value)
  expect_identical(upload_calls, 1L)
  expect_identical(sync_calls, 1L)
})
test_that("upload() converts labelled data before uploading when internals$labelled is TRUE", {
  proj_list <- REDCapSync:::TEST_CLASSIC
  proj_list$internals$labelled <- TRUE
  project <- REDCapSync_project$new(proj_list)
  labelled_calls <- 0L
  upload_calls <- 0L
  df_in <- data.frame(record_id = c("1", "2"), sex = c("Female", "Male"))
  df_converted <- data.frame(record_id = c("1", "2"), sex = c(0L, 1L))
  testthat::local_mocked_bindings(
    labelled_to_raw_form = function(form, project) {
      labelled_calls <<- labelled_calls + 1L
      expect_true(is.data.frame(form))
      expect_true(is.list(project))
      df_converted
    },
    upload_form_to_redcap = function(to_be_uploaded, project, batch_size) {
      upload_calls <<- upload_calls + 1L
      # Check upload gets the converted object
      expect_identical(to_be_uploaded, df_converted)
      invisible(TRUE)
    },
    sync_project = function(project, hard_check, ...) {
      expect_true(isTRUE(hard_check))
      project
    }
  )
  res <- withVisible(project$upload(df_in, batch_size = 500L))
  expect_true(res$value)
  expect_identical(labelled_calls, 1L)
  expect_identical(upload_calls, 1L)
})
test_that("upload() returns FALSE and does not call API if nothing to upload", {
  proj_list <- REDCapSync:::TEST_CLASSIC
  proj_list$internals$labelled <- FALSE
  project <- REDCapSync_project$new(proj_list)
  upload_calls <- 0L
  sync_calls <- 0L
  testthat::local_mocked_bindings(
    upload_form_to_redcap = function(...) {
      upload_calls <<- upload_calls + 1L
      stop("Should not be called for empty upload")
    },
    sync_project = function(project, ...) {
      sync_calls <<- sync_calls + 1L
      stop("Should not be called for empty upload")
    }
  )
  empty_df <- data.frame()
  expect_message(
    res <- withVisible(project$upload(empty_df)),
    "Nothing to upload"
  )
  expect_false(res$value)
  expect_identical(upload_calls, 0L)
  expect_identical(sync_calls, 0L)
})
