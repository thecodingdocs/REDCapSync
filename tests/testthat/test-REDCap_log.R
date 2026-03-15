tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
# clean_redcap_log (Internal)
test_that("clean_redcap_log removes duplicates", {
  redcap_log <- data.frame(
    timestamp = c("2024-01-15 10:30:00",
                  "2024-01-15 10:30:00",
                  "2024-01-15 10:35:00"),
    username = c("user1", "user1", "user2"),
    action = c("Update record 123", "Update record 123", "Create record 456"),
    details = c("Field updated", "Field updated", "New record"),
    record = c("123", "123", "456"),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_identical(nrow(result), 2L)
})
test_that("clean_redcap_log trims whitespace", {
  redcap_log <- data.frame(
    timestamp = "2024-01-15 10:30:00",
    username = "  user1  ",
    action = "  Update record 123  ",
    details = "  Field updated  ",
    record = "123",
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_false(grepl("^\\s|\\s$", result$username[1L]))
  expect_false(grepl("^\\s|\\s$", result$action[1L]))
  expect_false(grepl("^\\s|\\s$", result$details[1L]))
})
test_that("clean_redcap_log identifies record actions", {
  redcap_log <- data.frame(
    timestamp = c(
      "2024-01-15 10:30:00",
      "2024-01-15 10:35:00",
      "2024-01-15 10:40:00",
      "2024-01-15 10:45:00"
    ),
    username = c("user1", "user2", "user3", "user4"),
    action = c(
      "Update record 123",
      "Delete record 456",
      "Create record 789",
      "Lock/Unlock Record 321"
    ),
    details = c("Updated", "Deleted", "Created", "Locked"),
    record = c("123", "456", "789", "321"),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  # Check that record_id was extracted and action_type was set
  expect_all_true(!is.na(result$action_type[1L:4L]))
  expect_all_true(
    result$action_type[1L:4L] %in%
      c("Update", "Delete", "Create", "Lock/Unlock")
  )
})
test_that("clean_redcap_log handles Manage/Design actions", {
  redcap_log <- data.frame(
    timestamp = c(
      "2024-01-15 10:30:00",
      "2024-01-15 10:35:00"
    ),
    username = c("user1", "user2"),
    action = c("Manage/Design", "Manage/Design"),
    details = c(
      "Edit project field: field_name",
      "Create project field: new_field"
    ),
    record = c(NA, NA),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_true(any(result$action_type == "Metadata Change Major", na.rm = TRUE))
})
test_that("clean_redcap_log converts [survey respondent] to NA", {
  redcap_log <- data.frame(
    timestamp = c("2024-01-15 10:35:00", "2024-01-15 10:30:00"),
    username = c("[survey respondent]", "user1"),
    action = c("Update record 123", "Create record 456"),
    details = c("Survey submitted", "Created"),
    record = c("123", "456"),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_s3_class(result, "data.frame")
  expect_true(is.na(result$username[1L]))
  expect_identical(result$username[2L], "user1")
})
test_that("clean_redcap_log sets action_type column", {
  redcap_log <- data.frame(
    timestamp = "2024-01-15 10:30:00",
    username = "user1",
    action = "Update record 123",
    details = "Updated",
    record = "123",
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_true("action_type" %in% colnames(result))
  expect_false(all(is.na(result$action_type)))
})
test_that("clean_redcap_log handles export actions", {
  redcap_log <- data.frame(
    timestamp = c(
      "2024-01-15 10:30:00",
      "2024-01-15 10:35:00"
    ),
    username = c("user1", "user2"),
    action = c("Data export", "Download uploaded file"),
    details = c("Exported data", "Downloaded file"),
    record = c(NA, NA),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_true(all(result$action_type %in% c("Exports", NA)))
})
test_that("clean_redcap_log handles user actions", {
  redcap_log <- data.frame(
    timestamp = c(
      "2024-01-15 10:30:00",
      "2024-01-15 10:35:00"
    ),
    username = c("user1", "user2"),
    action = c("Add user test", "Edit user admin"),
    details = c("User added", "User edited"),
    record = c(NA, NA),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_true(all(result$action_type %in% c("Users", NA)))
})
test_that("clean_redcap_log handles no-changes actions", {
  redcap_log <- data.frame(
    timestamp = "2024-01-15 10:30:00",
    username = "user1",
    action = "Enable external module test",
    details = "Module enabled",
    record = NA,
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_identical(result$action_type[1L], "No Changes")
})
test_that("clean_redcap_log sorts by timestamp descending", {
  redcap_log <- data.frame(
    timestamp = c(
      "2024-01-15 10:30:00",
      "2024-01-15 10:45:00",
      "2024-01-15 10:35:00"
    ),
    username = c("user1", "user2", "user3"),
    action = c("Update record 1", "Update record 2", "Update record 3"),
    details = c("Updated", "Updated", "Updated"),
    record = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  # Should be sorted by timestamp descending
  expect_identical(result$timestamp[1L], "2024-01-15 10:45:00")
  expect_identical(result$timestamp[2L], "2024-01-15 10:35:00")
  expect_identical(result$timestamp[3L], "2024-01-15 10:30:00")
})
test_that("clean_redcap_log removes record_id column", {
  redcap_log <- data.frame(
    timestamp = "2024-01-15 10:30:00",
    username = "user1",
    action = "Update record 123",
    details = "Updated",
    record = "123",
    record_id = NA,
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_false("record_id" %in% colnames(result))
})
# redcap_log_labeler (Internal)
test_that("redcap_log_labeler works!", {
})
# analyze_log (Internal)
test_that("analyze_log works!", {
  project_name <- "TEST_CLASSIC"
  project <- mock_test_project(project_name)$.internal
  id_col <- project$metadata$id_col
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Create record (API) 99",
    details = NA,
    record = "99",
    action_type = "Create",
    stringsAsFactors = FALSE
  )
  expect_identical(analyze_log(interim_log, id_col)$length_updated_records, 1L)
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Create record (API) 99",
    details = NA,
    record = "99",
    action_type = c("Create", "Update"),
    stringsAsFactors = FALSE
  )
  expect_identical(analyze_log(interim_log, id_col)$length_updated_records, 1L)
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Create record (API) 99",
    details = NA,
    record = "99",
    action_type = "Delete",
    stringsAsFactors = FALSE
  )
  log_changes <- analyze_log(interim_log, id_col)
  expect_identical(log_changes$length_updated_records, 1L)
  expect_identical(log_changes$length_deleted_records, 1L)
  expect_identical(log_changes$updated_records, "99")
  expect_identical(log_changes$deleted_records, "99")
  expect_true(log_changes$refresh_data)
  expect_false(log_changes$refresh_metadata)
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Changes to metadata",
    details = NA,
    record = NA,
    action_type = "Metadata Change Major",
    stringsAsFactors = FALSE
  )
  expect_true(analyze_log(interim_log, id_col)$refresh_metadata)
  interim_log <- data.frame(
    timestamp = as.character(Sys.time()),
    username = "u1231",
    action = "Changes to metadata",
    details = NA,
    record = NA,
    action_type = "Metadata Change Minor",
    stringsAsFactors = FALSE
  )
  expect_true(analyze_log(interim_log, id_col)$refresh_metadata)
})
# log_change_messages (Internal)
test_that("log_change_messages works!", {
  log_changes <- list(
    hard_reset = FALSE,
    refresh_metadata = FALSE,
    refresh_metadata_major = FALSE,
    refresh_metadata_minor = FALSE,
    refresh_users = FALSE,
    refresh_data = FALSE,
    deleted_records = NULL,
    updated_records = NULL,
    renamed_records = NULL,
    comment_records = NULL,
    length_deleted_records = 0L,
    length_updated_records = 0L,
    length_renamed_records = 0L,
    length_comment_records = 0L
  )
  expect_message(log_change_messages(log_changes), "Up to date")
  log_changes$refresh_metadata <- TRUE
  expect_message(log_change_messages(log_changes), "Full update triggered")
  log_changes$refresh_metadata <- FALSE
  log_changes$renamed_records <- "1c"
  log_changes$length_renamed_records <- 1L
  expect_message(log_change_messages(log_changes), "Full update triggered")
  log_changes$renamed_records <- NULL
  log_changes$length_renamed_records <- 0L
  log_changes$length_comment_records <- 1L
  log_changes$length_deleted_records <- 1L
  log_changes$length_updated_records <- 1L
  log_changes$comment_records <- "comment_record"
  log_changes$deleted_records <- "deleted_record"
  log_changes$updated_records <- "updated_record"
  expect_message(log_change_messages(log_changes), "Deleted: deleted")
  expect_message(log_change_messages(log_changes), "Updated: updated")
  expect_message(log_change_messages(log_changes), "Comments: comment")
})
# generate_comment_table (Internal)
test_that("generate_comment_table works!", {
  redcap_log <- TEST_CLASSIC$redcap$log[10L, ]
  time_offset <- seq(from = 100000L, to = 1000000L, by = 100000L)
  redcap_log_comments <- data.frame(
    timestamp = rep(Sys.time(), 10L) - time_offset,
    username = "commenter5",
    action = "Manage/Design",
    details = c(
      generate_comments("Edit", "1", "var_multi_dropdown", "some comment"),
      generate_comments("Add", "2", "var_branching", "different comment"),
      generate_comments("Delete", "2", "var_branching", "A comment"),
      generate_comments("Add", "2", "var_branching", "A comment"),
      generate_comments("Add", "3", "var_multi_dropdown", "some comment"),
      generate_comments("Add", "4", "var_branching", "a comment"),
      generate_comments("Edit", "1", "var_multi_dropdown", NULL),
      generate_comments("Add", "5", "var_multi_dropdown", "another comment"),
      generate_comments("Add", "1", "var_branching", "some comment"),
      generate_comments("Add", "1", "var_multi_dropdown", "some comment")
    ),
    record = c("1", "1", "2", "2", "3", "4", "1", "5", "1", "1"),
    action_type = "Comment",
    stringsAsFactors = FALSE
  )
  comment_table <- generate_comment_table(redcap_log = redcap_log_comments)
  expect_contains(comment_table$comment_type, c("Add", "Edit", "Delete"))
  expect_all_false(is.na(comment_table$comment_type))
  expect_all_false(is.na(comment_table$comment_field))
  expect_all_false(is.na(comment_table$comment_details[-7L]))
  expect_scalar_na(comment_table$comment_details[7L])
  expect_data_frame(comment_table, nrows = 10L)
  comment_table <- generate_comment_table(redcap_log = redcap_log_comments,
                                          only_most_recent = TRUE)
  comment_table
})
