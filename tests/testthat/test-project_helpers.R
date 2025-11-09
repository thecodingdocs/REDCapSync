test_that("link_ helpers work", {
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
  project$links$redcap_home <- "https://fakeredcap.com/version/project"
  project$links$redcap_api <- "https://fakeredcap.com/version/project/api"
  project$links$redcap_api_playground <- "https://fakeredcap.com/version/project/api_playground"
  e <- new.env(parent = emptyenv())
  # link_API_token
  e$called_url <- NULL
  mockery::stub(
    link_API_token, "utils::browseURL",
    function(url) e$called_url <- url
  )
  link_API_token(project)
  expect_equal(e$called_url, project$links$redcap_api)
  # link_API_playground
  e$called_url <- NULL
  mockery::stub(
    link_API_playground, "utils::browseURL",
    function(url) e$called_url <- url
  )
  link_API_playground(project)
  expect_equal(e$called_url, project$links$redcap_api_playground)
  # link_REDCap_home
  e$called_url <- NULL
  mockery::stub(
    link_REDCap_home, "utils::browseURL",
    function(url) e$called_url <- url
  )
  link_REDCap_home(project)
  expect_equal(e$called_url, project$links$redcap_base)
  # link_REDCap_project
  e$called_url <- NULL
  mockery::stub(
    link_REDCap_project, "utils::browseURL",
    function(url) e$called_url <- url
  )
  link_REDCap_project(project)
  expect_equal(e$called_url, project$links$redcap_home)
})
test_that("link_REDCap_record works", {
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
  # link_API_token
  e$called_url <- NULL
  mockery::stub(
    link_REDCap_record, "utils::browseURL",
    function(url) e$called_url <- url
  )
  expect_equal(link_REDCap_record(project, text_only = TRUE), expected_link)
  expect_null(e$called_url) # does not call url
  e$called_url <- NULL
  link_REDCap_record(project, text_only = FALSE)
  expect_equal(e$called_url, expected_link)
  e$called_url <- NULL
  # link_REDCap_record(project,page = "2", text_only = TRUE)
})
