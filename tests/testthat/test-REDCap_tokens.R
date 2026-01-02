# is_valid_redcap_token ( Internal )
test_that("is_valid_redcap_token respects the rules of 32L hexadecimal", {
  expect_true(is_valid_redcap_token(generate_hex(32L)))
  expect_false(is_valid_redcap_token(NA))
  expect_false(is_valid_redcap_token(NULL))
  expect_false(is_valid_redcap_token(generate_hex(31L)))
  expect_false(is_valid_redcap_token(generate_hex(33L)))
  expect_false(is_valid_redcap_token(paste0(" ", generate_hex(31L))))
  expect_false(is_valid_redcap_token(paste0("J", generate_hex(31L))))
  expect_false(is_valid_redcap_token(paste0("_", generate_hex(31L))))
})
test_that("get_project_token checks_env", {
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
  token_name <- project$redcap$token_name
  token <- generate_hex(32L)
  withr::with_envvar(c(REDCapSync_TEST_PROJECT = token), {
    expect_identical(get_project_token(project), token)
    expect_no_error(get_project_token(project))
  })
  token <- generate_hex(2L)
  withr::with_envvar(c(REDCapSync_TEST_PROJECT = token), {
    expect_identical(get_project_token(project), token)
  })
  withr::with_envvar(c(REDCapSync_TEST_PROJECT = NULL), {
    expect_identical(get_project_token(project), "")
  })
})
# view_project_token ( Exported )
test_that("view_project_token works when no token set", {
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
  expect_true(project$internals$is_test)
  expect_message(view_project_token(project), "is not a valid 32-character")
})
test_that("view_project_token works when token is set", {
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
  token_name <- project$redcap$token_name
  token <- generate_hex(32L)
  withr::with_envvar(c(REDCapSync_TEST_PROJECT = token), {
    expect_message(view_project_token(project),
                   paste0("Never share your token: ", token))
  })
})
# test_project_token ( Exported )
test_that("test_project_token works when exportVersion returns version", {
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
  # Stub rcon to avoid creating a real connection and stub
  # exportVersion to simulate success
  mockery::stub(test_project_token,
                "redcapConnection",
                list(
                  projectInformation = function() {
                    list(project_id = project$redcap$project_id)
                  }
                ))
  mockery::stub(test_project_token, "exportVersion", "12.1.1")
  expect_message(test_project_token(project), "Connected to REDCap")
  out <- test_project_token(project)
  expect_true(out$internals$last_test_connection_outcome)
  expect_false(is.null(out$internals$last_test_connection_attempt))
  mockery::stub(test_project_token,
                "redcapConnection",
                list(
                  projectInformation = function() {
                    list(project_id = "5678")
                  }
                ))
  mockery::stub(test_project_token, "exportVersion", "12.1.1")
  expect_message(test_project_token(project),
                 "The REDCap project ID for TEST_PROJECT has changed")
})
test_that("test_project_token marks failure when exportVersion returns NULL", {
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
  # Stub rcon and simulate exportVersion failure
  mockery::stub(test_project_token, "rcon", function(project) {
    list()
  })
  mockery::stub(test_project_token, "redcapAPI::exportVersion", NULL)
  out <- test_project_token(project)
  expect_false(out$internals$last_test_connection_outcome)
  expect_false(is.null(out$internals$last_test_connection_attempt))
})
# is_hexadecimal ( Internal )
test_that("is_hexadecimal works!", {
  x <- generate_hex(length = 32L)
  expect_true(is_hexadecimal(x))
  expect_true(is_hexadecimal(x, length = 32L))
  expect_false(is_hexadecimal(x, length = 31L))
  x <- generate_hex(length = 30L)
  expect_true(is_hexadecimal(x, length = 30L))
  expect_false(is_hexadecimal(x, length = 31L))
  expect_true(is_hexadecimal("C234"))
  expect_true(is_hexadecimal("C"))
  expect_false(is_hexadecimal("C*"))
  expect_true(is_hexadecimal("abd3"))
  expect_true(is_hexadecimal("123"))
})
