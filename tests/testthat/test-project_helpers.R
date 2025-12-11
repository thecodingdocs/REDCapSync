test_that("update_project_links works", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      fake_cache
    }
  )
  project <- mock_project()
  expect_false(is.null(project$links$redcap_uri))
  expect_false(is.null(project$links$redcap_base))
  link_vector <- paste0("redcap_", .link_types)
  link_vector <- setdiff(link_vector, "redcap_base")
  #check null
  for (the_link in link_vector) {
    expect_null(project$links[[the_link]])
  }
  #do it!
  project <- update_project_links(project)
  pid_pattern <- paste0("pid=", project$redcap$project_id)
  version_pattern <- paste0("redcap_v", project$redcap$version)
  for (the_link in link_vector) {
    expect_false(is.null(project$links[[the_link]]))
    expect_true(grepl(pid_pattern, project$links[[the_link]]))
    expect_true(grepl(version_pattern, project$links[[the_link]]))
  }
  # version changed!
  version_old <- project$redcap$version
  version_new <- "14.2.3"
  project$redcap$version <- version_new
  project <- update_project_links(project)
  expect_equal(project$redcap$version, version_new)
  pid_pattern <- paste0("pid=", project$redcap$project_id)
  version_pattern <- paste0("redcap_v", project$redcap$version)
  for (the_link in link_vector) {
    expect_false(is.null(project$links[[the_link]]))
    expect_true(grepl(pid_pattern, project$links[[the_link]]))
    expect_true(grepl(version_pattern, project$links[[the_link]]))
  }
})
# get_project_url ( Exported )
test_that("get_project_url works", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      fake_cache
    }
  )
  project <- mock_project()
  project <- update_project_links(project)
  e <- new.env(parent = emptyenv())
  # get_project_url
  e$called_url <- NULL
  mockery::stub(get_project_url, "utils::browseURL", function(url) {
    e$called_url <- url
  })
  for (link_type in .link_types) {
    e$called_url <- NULL
    get_project_url(project, link_type = link_type, open_browser = TRUE)
    expect_equal(e$called_url, project$links[[paste0("redcap_", link_type)]])
    out <- get_project_url(project, link_type = link_type, open_browser = FALSE)
    expect_equal(out, project$links[[paste0("redcap_", link_type)]])
  }
})
# get_record_url ( Exported )
test_that("get_record_url works", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      fake_cache
    }
  )
  project <- mock_project()
  project$links$redcap_base <- "https://fakeredcap.com/"
  expected_link <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/DataEntry/record_home.php?pid=",
    project$redcap$project_id
  )
  e <- new.env(parent = emptyenv())
  # get_project_url
  e$called_url <- NULL
  mockery::stub(get_record_url, "utils::browseURL", function(url) {
    e$called_url <- url
  })
  expect_equal(get_record_url(project, open_browser = FALSE), expected_link)
  expect_null(e$called_url) # does not call url
  e$called_url <- NULL
  get_record_url(project, open_browser = TRUE)
  expect_equal(e$called_url, expected_link)
  e$called_url <- NULL
  # get_record_url(project,page = "2", text_only = TRUE)
})
test_that("deidentify_data_list works", {
  project <- TEST_CLASSIC
  data_list <- merge_non_repeating(TEST_CLASSIC, "merged")
  data_list$metadata$fields$field_type_R <- NA
  data_list$metadata$fields$in_original_redcap <- NA
  id_cols <- data_list$metadata$form_key_cols |>
    unlist() |>
    unique()
  fields <- data_list$metadata$fields
  initial_identifiers <- fields$field_name[which(
    fields$identifier == "y" |
      fields$text_validation_type_or_show_slider_number %in%
      .redcap_possible_id_fields_strict
  )]
  free_text_rows <- which(
    fields$field_type == "notes" |
      (
        fields$field_type == "text" &
          is.na(fields$text_validation_type_or_show_slider_number)
      ) &
      !fields$field_name %in% id_cols
  )
  free_text_fields <- fields$field_name[free_text_rows]
  expect_all_true(initial_identifiers %in% colnames(data_list$data$merged))
  no_ids <- data_list |> deidentify_data_list(exclude_identifiers = TRUE,
                                               exclude_free_text = FALSE)
  expect_all_false(initial_identifiers %in% colnames(no_ids$data$merged))
  expect_all_true(free_text_fields %in% colnames(data_list$data$merged))
  no_free_text <- data_list |>
    deidentify_data_list(exclude_identifiers = FALSE,
                         exclude_free_text = TRUE)
  expect_all_false(initial_identifiers %in% colnames(no_free_text$data$merged))
  keep_rows <-
    which(!data_list$metadata$fields$field_name %in% initial_identifiers)
  data_list$metadata$fields <- data_list$metadata$fields[keep_rows, ]
  keep_cols <- which(!colnames(data_list$data$merged) %in% initial_identifiers)
  data_list$data$merged <- data_list$data$merged[, keep_cols]
  expect_warning(
    data_list |> deidentify_data_list(
      exclude_identifiers = TRUE,
      exclude_free_text = FALSE
    ),
    "You have no identifiers marked"
  )
})
test_that("deidentify_data_list works", {
  project <- TEST_CLASSIC
  data_list <- merge_non_repeating(TEST_CLASSIC, "merged")
  data_list <- data_list |> metadata_add_default_cols()
  fields <- data_list$metadata$fields
  merged <- data_list$data$merged
  expect_all_true(
    c(
      "var_birth_date",
      "var_text_date_dmy",
      "var_text_date_mdy",
      "var_text_date_ymd"
    ) %in% colnames(merged)
  )
  expect_error(deidentify_data_list(data_list = data_list, date_handling = "1"))
  # 'none'
  merged_none <- deidentify_data_list(
    data_list = data_list,
    # exclude_identifiers = FALSE,
    date_handling = "none"
  )$merged
  expect_all_true(
    c(
      "var_birth_date",
      "var_text_date_dmy",
      "var_text_date_mdy",
      "var_text_date_ymd"
    ) %in% colnames(merged_none)
  )
  # 'exclude_dates'
  merged_exclude_dates <-
    deidentify_data_list(
      data_list = data_list, date_handling = "exclude_dates")$merged
  expect_all_false(
    c(
      "var_birth_date",
      "var_text_date_dmy",
      "var_text_date_mdy",
      "var_text_date_ymd"
    ) %in% colnames(merged_exclude_dates)
  )
  # 'random_shift_by_record'
  merged_random_shift_by_record <-
    deidentify_data_list(
      data_list = data_list, date_handling = "random_shift_by_record")$merged
  expect_all_true(
    c(
      "var_birth_date",
      "var_text_date_dmy",
      "var_text_date_mdy",
      "var_text_date_ymd"
    ) %in% colnames(merged_random_shift_by_record)
  )
  expect_all_false(
    merged_random_shift_by_record$var_text_date_dmy == merged$var_text_date_dmy)
  time_check1 <- as.Date(merged$var_text_date_dmy) -
    as.Date(merged$var_birth_date)
  time_check2 <- as.Date(merged_random_shift_by_record$var_text_date_dmy) -
    as.Date(merged_random_shift_by_record$var_birth_date)
  expect_all_true(time_check1 == time_check2) #math is same
  as.Date(merged$var_birth_date) -
    as.Date(merged_random_shift_by_record$var_birth_date)
  # 'random_shift_by_project'
  # merged_random_shift_by_project <- deidentify_data_list(
  #   data_list = data_list,date_handling = "random_shift_by_project")
  # # 'zero_by_record'
  # merged_zero_by_record <- deidentify_data_list(
  #   data_list = data_list,date_handling = "zero_by_record")
  # # 'zero_by_project'
  # merged_zero_by_project <- deidentify_data_list(
  #   data_list = data_list,date_handling = "zero_by_project")
})
test_that("get_min_dates works", {
  # construct minimal data_list with date fields across forms
  data_list <- list(
    data = list(
      form1 = data.frame(
        record_id = c("1", "2"),
        date1 = c("2020-01-01", "2020-01-05"),
        stringsAsFactors = FALSE
      ),
      form2 = data.frame(
        record_id = c("2", "3"),
        date2 = c("2020-02-01", "2020-01-15"),
        stringsAsFactors = FALSE
      )
    ),
    metadata = list(
      fields = data.frame(
        field_name = c("record_id", "date1", "date2"),
        field_type_R = c(NA, "date", "date"),
        stringsAsFactors = FALSE
      ),
      form_key_cols = list(record_id = "record_id")
    )
  )
  out <- get_min_dates(data_list)
  # basic structure checks
  checkmate::expect_data_frame(out, nrows = 3, ncols = 2)
  expect_true(all(c("record_id", "date") %in% colnames(out)))
  # values: min date per record across forms
  expect_equal(as.character(out$date[match("1", out$record_id)]), "2020-01-01")
  expect_equal(as.character(out$date[match("2", out$record_id)]), "2020-01-05")
  expect_equal(as.character(out$date[match("3", out$record_id)]), "2020-01-15")
})
# normalize_redcap ( Internal )
test_that("normalize_redcap works with classic project", {
  project <- TEST_CLASSIC
  denormalized <- readRDS(
    test_path("fixtures", "TEST_CLASSIC_denormalized.rds"))
  result <- normalize_redcap(denormalized, project, labelled = TRUE)
  expect_true(is.list(result))
  form_names <- names(result)
  expected_forms <- project$metadata$forms$form_name
  expect_all_true(expected_forms %in% form_names)
  expect_all_true(unlist(lapply(result, is.data.frame)))
  id_col <- project$metadata$id_col
  fields <- project$metadata$fields
  expect_true(id_col %in% colnames(result$text))
  expect_all_true(
    fields$field_name[which(fields$form_name == "text")] %in%
      colnames(result$text))
  expect_all_true(
    fields$field_name[
      which(fields$form_name == "other" &
              (!fields$field_type %in% c("checkbox","descriptive")))] %in%
      colnames(result$other)
  )
  fields <- fields[which(fields$field_name != id_col), ]
  expect_all_false(
    fields$field_name[which(fields$form_name == "text")] %in%
      colnames(result$other))
  expect_all_false(
    fields$field_name[
      which(fields$form_name == "other" &
              (!fields$field_type %in% c("checkbox","descriptive")))] %in%
      colnames(result$text)
  )
})
# clean_data_list ( Internal )
# get_key_col_list ( Internal )
# normalize_redcap ( Internal )
# sort_redcap_log ( Internal )
# clean_redcap_log ( Internal )
# clean_redcap_log ( Internal )
# clean_redcap_log ( Internal )
# clean_redcap_log ( Internal )
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
  expect_equal(nrow(result), 2)
})
test_that("clean_redcap_log trims whitespace", {
  redcap_log <- data.frame(
    timestamp = c("2024-01-15 10:30:00"),
    username = c("  user1  "),
    action = c("  Update record 123  "),
    details = c("  Field updated  "),
    record = c("123"),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_false(grepl("^\\s|\\s$", result$username[1]))
  expect_false(grepl("^\\s|\\s$", result$action[1]))
  expect_false(grepl("^\\s|\\s$", result$details[1]))
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
  expect_true(all(!is.na(result$action_type[1:4])))
  expect_true(all(
    result$action_type[1:4] %in% c("Update", "Delete", "Create", "Lock/Unlock")
  ))
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
  expect_true(is.na(result$username[1]))
  expect_equal(result$username[2], "user1")
})
test_that("clean_redcap_log sets action_type column", {
  redcap_log <- data.frame(
    timestamp = c("2024-01-15 10:30:00"),
    username = c("user1"),
    action = c("Update record 123"),
    details = c("Updated"),
    record = c("123"),
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
    timestamp = c("2024-01-15 10:30:00"),
    username = c("user1"),
    action = c("Enable external module test"),
    details = c("Module enabled"),
    record = c(NA),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_equal(result$action_type[1], "No Changes")
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
  expect_equal(result$timestamp[1], "2024-01-15 10:45:00")
  expect_equal(result$timestamp[2], "2024-01-15 10:35:00")
  expect_equal(result$timestamp[3], "2024-01-15 10:30:00")
})
test_that("clean_redcap_log removes record_id column", {
  redcap_log <- data.frame(
    timestamp = c("2024-01-15 10:30:00"),
    username = c("user1"),
    action = c("Update record 123"),
    details = c("Updated"),
    record = c("123"),
    record_id = c(NA),
    stringsAsFactors = FALSE
  )
  result <- clean_redcap_log(redcap_log)
  expect_false("record_id" %in% colnames(result))
})
# check_missing_codes ( Internal )
