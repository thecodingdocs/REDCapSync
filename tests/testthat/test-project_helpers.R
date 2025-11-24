test_that("update_project_links works", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      return(fake_cache)
    }
  )
  project <- mock_project()
  expect_false(is.null(project$links$redcap_uri))
  expect_false(is.null(project$links$redcap_base))
  link_vector <- paste0("redcap_",.link_types)
  link_vector <- setdiff(link_vector,"redcap_base")
  #check null
  for(the_link in link_vector){
    expect_null(project$links[[the_link]])
  }
  #do it!
  project <- update_project_links(project)
  pid_pattern <-paste0("pid=",project$redcap$project_id)
  version_pattern <-paste0("redcap_v",project$redcap$version)
  for(the_link in link_vector){
    expect_false(is.null(project$links[[the_link]]))
    expect_true(grepl(pid_pattern,project$links[[the_link]]))
    expect_true(grepl(version_pattern,project$links[[the_link]]))
  }
  # version changed!
  version_old <- project$redcap$version
  version_new <- "14.2.3"
  project$redcap$version <- version_new
  project <- update_project_links(project)
  expect_equal(project$redcap$version,version_new)
  pid_pattern <-paste0("pid=",project$redcap$project_id)
  version_pattern <-paste0("redcap_v",project$redcap$version)
  for(the_link in link_vector){
    expect_false(is.null(project$links[[the_link]]))
    expect_true(grepl(pid_pattern,project$links[[the_link]]))
    expect_true(grepl(version_pattern,project$links[[the_link]]))
  }
})
# get_project_url ( Exported )
test_that("get_project_url works", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      return(fake_cache)
    }
  )
  project <- mock_project()
  project <- update_project_links(project)
  e <- new.env(parent = emptyenv())
  # get_project_url
  e$called_url <- NULL
  mockery::stub(
    get_project_url, "utils::browseURL",
    function(url) e$called_url <- url
  )
  for(link_type in .link_types){
    e$called_url <- NULL
    get_project_url(project, link_type = link_type, open_browser = TRUE)
    expect_equal(e$called_url, project$links[[paste0("redcap_",link_type)]])
    out <- get_project_url(project, link_type = link_type, open_browser = FALSE)
    expect_equal(out, project$links[[paste0("redcap_",link_type)]])
  }
})
# get_record_url ( Exported )
test_that("get_record_url works", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  fake_cache_location <- file.path(test_dir, "fake_cache")
  local_mocked_bindings(
    get_cache = function(...) {
      fake_cache <- hoardr::hoard()
      fake_cache$cache_path_set(full_path = fake_cache_location)
      fake_cache$mkdir()
      return(fake_cache)
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
  mockery::stub(
    get_record_url, "utils::browseURL",
    function(url) e$called_url <- url
  )
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
  data_list <- merge_non_repeating(TEST_CLASSIC,"merged")
  data_list$metadata$fields$field_type_R <- NA
  data_list$metadata$fields$in_original_redcap <- NA
  id_cols <- data_list$metadata$form_key_cols %>%
    unlist() %>%
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
  no_ids <- data_list %>% deidentify_data_list(
    exclude_identifiers = TRUE,
    exclude_free_text = FALSE
  )
  expect_all_false(initial_identifiers %in% colnames(no_ids$data$merged))
  expect_all_true(free_text_fields %in% colnames(data_list$data$merged))
  no_free_text <- data_list %>% deidentify_data_list(
    exclude_identifiers = FALSE,
    exclude_free_text = TRUE
  )
  expect_all_false(initial_identifiers %in% colnames(no_free_text$data$merged))
  keep_rows <- which(!data_list$metadata$fields$field_name %in% initial_identifiers)
  data_list$metadata$fields <- data_list$metadata$fields[keep_rows,]
  keep_cols <- which(!colnames(data_list$data$merged) %in% initial_identifiers)
  data_list$data$merged <- data_list$data$merged[,keep_cols]
  expect_warning(data_list %>% deidentify_data_list(
    exclude_identifiers = TRUE,
    exclude_free_text = FALSE
  ),"You have no identifiers marked")
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
# clean_data_list ( Internal )
# get_key_col_list ( Internal )
# normalize_redcap ( Internal )
# sort_redcap_log ( Internal )
# clean_redcap_log ( Internal )
test_that("clean_redcap_log works", {
})
# check_missing_codes ( Internal )
