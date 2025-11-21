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
# test_that("deidentify_data_list works", {
#   project <- TEST_CLASSIC
#   project_summary <- project %>% generate_project_summary()
#   merged <- project_summary$merged %>% all_character_cols()
#   merged_deidentified <-
# })
# clean_data_list ( Internal )
# get_key_col_list ( Internal )
# normalize_redcap ( Internal )
# sort_redcap_log ( Internal )
# clean_redcap_log ( Internal )
# check_missing_codes ( Internal )
