test_that("internal constants are correct", {
  expect_equal(.token_prefix, "REDCapSync_")
  expect_equal(.TEST_classic_token, "FAKE32TESTTOKENCLASSIC1111111111")
  expect_equal(.TEST_repeating_token, "FAKE32TESTTOKENREPEATING22222222")
  expect_equal(.TEST_longitudinal_token, "FAKE32TESTTOKENLONGITUDINAL33333")
  expect_equal(.TEST_multiarm_token, "FAKE32TESTTOKENMULTIARM444444444")
})
# is_valid_redcap_token ( Internal )
test_that("is_valid_redcap_token respects the rules of 32L hexidecimal", {
  expect_true(is_valid_redcap_token(generate_hex(32)))
  expect_false(is_valid_redcap_token(NA))
  expect_false(is_valid_redcap_token(NULL))
  expect_false(is_valid_redcap_token(generate_hex(31)))
  expect_false(is_valid_redcap_token(generate_hex(33)))
  expect_false(is_valid_redcap_token(paste0(" ", generate_hex(31))))
  expect_false(is_valid_redcap_token(paste0("J", generate_hex(31))))
  expect_false(is_valid_redcap_token(paste0("_", generate_hex(31))))
  expect_false(is_valid_redcap_token(.TEST_classic_token))
  expect_true(is_valid_redcap_token(.TEST_classic_token, is_a_test = TRUE))
  expect_true(is_valid_redcap_token(.TEST_repeating_token, is_a_test = TRUE))
  expect_true(is_valid_redcap_token(.TEST_longitudinal_token, is_a_test = TRUE))
  expect_true(is_valid_redcap_token(.TEST_multiarm_token, is_a_test = TRUE))
  expect_false(is_valid_redcap_token(generate_hex(32), is_a_test = TRUE))
})
test_that("get_project_token checks_env", {
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
  token_name <- get_redcap_token_name(project)
  token <- generate_hex(32)
  withr::with_envvar(c(REDCapSync_TEST_PROJECT = token), {
    expect_equal(get_project_token(project), token)
    expect_no_error(get_project_token(project))
  })
  token <- generate_hex(2)
  withr::with_envvar(c(REDCapSync_TEST_PROJECT = token), {
    expect_equal(get_project_token(project), token)
  })
  withr::with_envvar(c(REDCapSync_TEST_PROJECT = NULL), {
    expect_equal(get_project_token(project), "")
  })
})
# get_redcap_token_name ( Internal )
test_that("get_redcap_token_name works", {
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
  expect_equal(get_redcap_token_name(project), "REDCapSync_TEST_PROJECT")
})
# view_project_token ( Exported )
test_that("view_project_token works when no token set", {
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
  expect_message(view_project_token(project), "Never share your token:")
})
test_that("view_project_token works when token is set", {
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
  token_name <- get_redcap_token_name(project)
  token <- generate_hex(32)
  withr::with_envvar(c(REDCapSync_TEST_PROJECT = token), {
    expect_message(view_project_token(project),
                   paste0("Never share your token: ", token))
  })
})
# get_test_token ( Internal )
test_that("get_test_token works!", {
  expect_equal(get_test_token("TEST_classic"), .TEST_classic_token)
  expect_equal(get_test_token("TEST_repeating"), .TEST_repeating_token)
  expect_equal(get_test_token("TEST_longitudinal"), .TEST_longitudinal_token)
  expect_equal(get_test_token("TEST_multiarm"), .TEST_multiarm_token)
  expect_error(get_test_token("INVALID_SHORT_NAME"))
  expect_error(get_test_token(1213123))
  expect_error(get_test_token(c("TEST_classic", "TEST_repeating")))
})
# test_project_token ( Exported )
test_that("test_project_token succeeds when exportVersion returns a version (no API calls)", {
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
  # Stub project_rcon to avoid creating a real connection and stub exportVersion to simulate success
  mockery::stub(test_project_token, "project_rcon", function(project) list())
  mockery::stub(test_project_token, "redcapAPI::exportVersion", "12.1.1")
  out <- test_project_token(project, launch_browser = FALSE)
  expect_true(out$internals$last_test_connection_outcome)
  expect_true(!is.null(out$internals$last_test_connection_attempt))
})
test_that("test_project_token marks failure when exportVersion returns NULL (no API calls)", {
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
  # Stub project_rcon and simulate exportVersion failure
  mockery::stub(test_project_token, "project_rcon", function(project) list())
  mockery::stub(test_project_token, "redcapAPI::exportVersion", NULL)
  out <- test_project_token(project, launch_browser = FALSE)
  expect_false(out$internals$last_test_connection_outcome)
  expect_true(!is.null(out$internals$last_test_connection_attempt))
})
test_that("test_project_token only launches browser when launch_browser = TRUE", {
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
  project$links$redcap_api <- "https://api.example.com"
  project$links$redcap_base <- "https://base.example.com"
  # Stub dependencies to force version_error = TRUE
  mockery::stub(test_project_token, "project_rcon", function(project) list())
  mockery::stub(test_project_token, "redcapAPI::exportVersion", NULL)
  # Environment to capture URL calls (CRAN-safe)
  e <- new.env(parent = emptyenv())
  e$called_url <- NULL
  mockery::stub(
    test_project_token, "utils::browseURL",
    function(url) e$called_url <- url
  )
  # Case 1: launch_browser = TRUE → call browseURL with api
  e$called_url <- NULL
  test_project_token(project, launch_browser = TRUE)
  expect_equal(e$called_url, "https://api.example.com")
  # Case 2: launch_browser = FALSE → do NOT call browseURL
  e$called_url <- NULL
  test_project_token(project, launch_browser = FALSE)
  expect_null(e$called_url)
})
# is_hexadecimal ( Internal )
test_that("is_hexadecimal works!",{
  x<-generate_hex(length = 32)
  expect_true(is_hexadecimal(x))
  expect_true(is_hexadecimal(x,length = 32))
  expect_false(is_hexadecimal(x,length = 31))
  x<-generate_hex(length = 30)
  expect_true(is_hexadecimal(x,length = 30))
  expect_false(is_hexadecimal(x,length = 31))
  expect_true(is_hexadecimal("C234"))
  expect_true(is_hexadecimal("C"))
  expect_false(is_hexadecimal("C*"))
  expect_true(is_hexadecimal("abd3"))
  expect_true(is_hexadecimal("123"))
})
