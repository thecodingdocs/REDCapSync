test_that("upload_form_to_redcap() calls REDCapR::redcap_write expected args", {
  project <- list(
    links = list(redcap_uri = "https://redcap.fake.edu/api/")
  )
  to_be_uploaded <- data.frame(
    record_id = c("1", "2"),
    age = c(50L, 60L),
    stringsAsFactors = FALSE
  )
  captured <- list()
  # Stub token lookup so we don't depend on env vars
  mockery::stub(upload_form_to_redcap, "get_project_token", function(project) {
    "0123456789ABCDEF0123456789ABCDEF"
  })
  # Stub the network call and capture args
  mockery::stub(upload_form_to_redcap, "REDCapR::redcap_write", function(...) {
    captured <<- list(...)
    list(success = TRUE)
  })
  result <- upload_form_to_redcap(
    to_be_uploaded = to_be_uploaded,
    project = project,
    batch_size = 123L
  )
  expect_true(isTRUE(result$success))
  # Check behavior of RosyUtils::all_character_cols()
  expect_true(is.data.frame(captured$ds_to_write))
  expect_true(all(vapply(captured$ds_to_write, is.character, logical(1L))))
  expect_identical(captured$batch_size, 123L)
  expect_identical(captured$interbatch_delay, 0.2)
  expect_false(captured$continue_on_error)
  expect_identical(captured$redcap_uri, "https://redcap.fake.edu/api/")
  expect_identical(captured$token, "0123456789ABCDEF0123456789ABCDEF")
  expect_true(captured$overwrite_with_blanks)
})
