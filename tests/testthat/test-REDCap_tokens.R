test_that("internal constants are correct", {
  expect_equal(.token_prefix, "REDCapSync_")
  expect_equal(.TEST_classic_token, "FAKE32TESTTOKENCLASSIC1111111111")
  expect_equal(.TEST_repeating_token, "FAKE32TESTTOKENREPEATING22222222")
  expect_equal(.TEST_longitudinal_token, "FAKE32TESTTOKENLONGITUDINAL33333")
  expect_equal(.TEST_multiarm_token, "FAKE32TESTTOKENMULTIARM444444444")
})
test_that("get_test_token works correctly", {
  expect_equal(get_test_token("TEST_classic"), .TEST_classic_token)
  expect_equal(get_test_token("TEST_repeating"), .TEST_repeating_token)
  expect_equal(get_test_token("TEST_longitudinal"), .TEST_longitudinal_token)
  expect_equal(get_test_token("TEST_multiarm"), .TEST_multiarm_token)
  expect_error(get_test_token("INVALID_SHORT_NAME"))
  expect_error(get_test_token(1213123))
  expect_error(get_test_token(c("TEST_classic", "TEST_repeating")))
})
test_that("is_valid_REDCap_token respects the rules of 32L hexidecimal", {
  expect_true(is_valid_REDCap_token(generate_hex(32)))
  expect_false(is_valid_REDCap_token(NA))
  expect_false(is_valid_REDCap_token(NULL))
  expect_false(is_valid_REDCap_token(generate_hex(31)))
  expect_false(is_valid_REDCap_token(generate_hex(33)))
  expect_false(is_valid_REDCap_token(paste0(" ", generate_hex(31))))
  expect_false(is_valid_REDCap_token(paste0("J", generate_hex(31))))
  expect_false(is_valid_REDCap_token(paste0("_", generate_hex(31))))
  expect_false(is_valid_REDCap_token(.TEST_classic_token))
  expect_true(is_valid_REDCap_token(.TEST_classic_token, is_a_test = TRUE))
  expect_true(is_valid_REDCap_token(.TEST_repeating_token, is_a_test = TRUE))
  expect_true(is_valid_REDCap_token(.TEST_longitudinal_token, is_a_test = TRUE))
  expect_true(is_valid_REDCap_token(.TEST_multiarm_token, is_a_test = TRUE))
  expect_false(is_valid_REDCap_token(generate_hex(32), is_a_test = TRUE))
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
  token_name <- get_REDCap_token_name(project)
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
test_that("get_REDCap_token_name works", {
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
  expect_equal(get_REDCap_token_name(project), "REDCapSync_TEST_PROJECT")
})
# test_that("set_REDCap_token sets a new token", {
#   project <- mock_project()
#   mockery::stub(set_REDCap_token, "readline", .TEST_classic_token)
#   set_REDCap_token(project, ask = FALSE)
#   token_name <- get_REDCap_token_name(project)
#   expect_equal(Sys.getenv(token_name), .TEST_classic_token)
# })
# test_that("set_REDCap_token handles existing valid token", {
#   project <- mock_project("TEST_classic")
#
#   # Set an existing valid token
#   Sys.setenv(REDCapSync_TEST_classic = .TEST_classic_token)
#
#   # Mock user input to not change the token
#   stub(set_REDCap_token, "utils::menu", 2)
#
#   expect_message(set_REDCap_token(project, ask = TRUE), "You already have a valid token in your R session")
#   token_name <- get_REDCap_token_name(project)
#   expect_equal(Sys.getenv(token_name), .TEST_classic_token)
# })
# test_that("set_REDCap_token changes existing token when user confirms", {
#   project <- mock_project("TEST_classic")
#
#   # Set an existing valid token
#   Sys.setenv(REDCapSync_TEST_classic = .TEST_classic_token)
#
#   # Mock user input to change the token
#   stub(set_REDCap_token, "utils::menu", 1)
#   stub(set_REDCap_token, "readline", "NEW_FAKE32TESTTOKENCLASSIC1111111111")
#
#   set_REDCap_token(project, ask = TRUE)
#   token_name <- get_REDCap_token_name(project)
#   expect_equal(Sys.getenv(token_name), "NEW_FAKE32TESTTOKENCLASSIC1111111111")
# })
# test_that("set_REDCap_token validates the token", {
#   project <- mock_project("TEST_classic")
#
#   # Mock user input for an invalid token
#   stub(set_REDCap_token, "readline", "INVALID_TOKEN")
#
#   expect_error(set_REDCap_token(project, ask = FALSE), "The token is not a valid test token.")
# })
#
# test_that("set_REDCap_token handles test tokens", {
#   project <- mock_project("TEST_classic")
#
#   # Mock user input for a test token
#   stub(set_REDCap_token, "readline", .TEST_classic_token)
#
#   set_REDCap_token(project, ask = FALSE)
#   token_name <- get_REDCap_token_name(project)
#   expect_equal(Sys.getenv(token_name), .TEST_classic_token)
# })
#
# test_that("set_REDCap_token handles invalid project object", {
#   expect_error(set_REDCap_token(NULL), "project must be a valid REDCap database object")
# })
#
# test_that("set_REDCap_token handles missing API link", {
#   project <- mock_project("TEST_classic")
#   project$links$redcap_API <- NULL
#
#   expect_error(set_REDCap_token(project, ask = FALSE), "REDCap API link is missing in the project object")
# })
