tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
# get_project_token (Internal)
test_that("get_project_token works!", {
  project <- mock_test_project()$.internal
  token_name <- project$token_name
  token <- generate_hex(32L)
  withr::with_envvar(c(REDCAPSYNC_TEST_CLASSIC = token), {
    expect_identical(get_project_token(project), token)
    expect_no_error(get_project_token(project))
    expect_message(get_project_token(project = project, silent = FALSE),
                   "Valid token for TEST_CLASSIC from envvar")
  })
  token <- generate_hex(2L)
  withr::with_envvar(c(REDCAPSYNC_TEST_CLASSIC = token), {
    expect_identical(get_project_token(project), token)
    expect_message(get_project_token(project = project, silent = FALSE),
                   "is not a valid 32-character hexadecimal value")
  })
  withr::with_envvar(c(REDCAPSYNC_TEST_CLASSIC = "BAD"), {
    expect_identical(get_project_token(project), "BAD")
    expect_message(get_project_token(project = project, silent = FALSE),
                   "is not a valid 32-character hexadecimal value")
  })
})
# is_hexadecimal (Internal)
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
  expect_false(is_hexadecimal(NA))
  expect_false(is_hexadecimal(""))
  expect_error(is_hexadecimal())
})
# is_valid_redcap_token (Internal)
test_that("is_valid_redcap_token works!", {
  expect_true(is_valid_redcap_token(generate_hex(32L)))
  expect_false(is_valid_redcap_token(NA))
  expect_false(is_valid_redcap_token(NULL))
  expect_false(is_valid_redcap_token(""))
  expect_false(is_valid_redcap_token(generate_hex(31L)))
  expect_false(is_valid_redcap_token(generate_hex(33L)))
  expect_false(is_valid_redcap_token(paste0(" ", generate_hex(31L))))
  expect_false(is_valid_redcap_token(paste0("J", generate_hex(31L))))
  expect_false(is_valid_redcap_token(paste0("_", generate_hex(31L))))
})
# test_project_token (Internal)
test_that("test_project_token works!", {
  # check with API. move to API section?
})
test_that("test_project_token works when exportVersion returns version", {
  project <- mock_test_project()$.internal
  withr::local_envvar(REDCAPSYNC_TEST_CLASSIC = generate_hex(length = 32L))
  # exportVersion to simulate success
  local_mocked_bindings(
    redcapConnection = function(...) {
      list(
        projectInformation = function() {
          list(project_id = project$redcap$project_id)
        }
      )
    },
    exportVersion = function(...) "12.1.1"
  )
  expect_message(test_project_token(project), "Connected to REDCap")
  out <- test_project_token(project)
  expect_true(out$internals$last_test_connection_outcome)
  expect_false(is.null(out$internals$last_test_connection_attempt))
  local_mocked_bindings(
    redcapConnection = function(...) {
      list(
        projectInformation = function() {
          list(project_id = "5678")
        }
      )
    },
    exportVersion = function(...) "12.1.1"
  )
  regexp <- "The REDCap project ID for TEST_CLASSIC has changed"
  expect_error(test_project_token(project), regexp = regexp)
})
test_that("test_project_token marks failure when exportVersion returns NULL", {
  project <- mock_test_project()$.internal
  withr::local_envvar(REDCAPSYNC_TEST_CLASSIC = generate_hex(length = 32L))
  local_mocked_bindings(
    redcapConnection = function(...) list(),
    exportVersion = function(...) NULL
  )
  out <- test_project_token(project)
  expect_false(out$internals$last_test_connection_outcome)
  expect_false(is.null(out$internals$last_test_connection_attempt))
})
# update_project_links (Internal)
test_that("update_project_links works!", {
  project <- mock_test_project()$.internal
  expect_false(is.null(project$links$redcap_uri))
  expect_false(is.null(project$links$redcap_base))
  link_vector <- paste0("redcap_", LINK_TYPES)
  link_vector <- setdiff(link_vector, "redcap_base")
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
  expect_identical(project$redcap$version, version_new)
  pid_pattern <- paste0("pid=", project$redcap$project_id)
  version_pattern <- paste0("redcap_v", project$redcap$version)
  for (the_link in link_vector) {
    expect_false(is.null(project$links[[the_link]]))
    expect_true(grepl(pid_pattern, project$links[[the_link]]))
    expect_true(grepl(version_pattern, project$links[[the_link]]))
  }
})
# has_keyring_token (Internal)
test_that("has_keyring_token returns NULL when no projects exist", {
  local_mocked_bindings(
    get_projects = function(...) BLANK_PROJECT_DETAILS
  )
  expect_null(has_keyring_token())
})
test_that("has_keyring_token returns NULL for missing project names", {
  projects <- data.frame(project_name = "TEST_CLASSIC",
                         stringsAsFactors = FALSE)
  local_mocked_bindings(
    get_projects = function(...) projects
  )
  expect_null(has_keyring_token("MISSING_PROJECT"))
})
test_that("has_keyring_token returns NULL when keyring is invalid", {
  skip_if_not_installed("keyring")
  projects <- data.frame(project_name = "TEST_CLASSIC",
                         stringsAsFactors = FALSE)
  local_mocked_bindings(
    get_projects = function(...) projects,
    is_valid_keyring = function(...) FALSE
  )
  expect_null(has_keyring_token("TEST_CLASSIC"))
})
# has_envvar_token (Internal)
test_that("has_envvar_token works", {
  withr::local_envvar(c(
    REDCAPSYNC_TEST_CLASSIC = NA,
    REDCAPSYNC_TEST_DATA = NA,
    REDCAPSYNC_TEST_LONGITUDINAL = NA
  ))
  a <- mock_test_project("TEST_CLASSIC")$.internal |> extract_project_details()
  b <- mock_test_project("TEST_DATA")$.internal |> extract_project_details()
  c <- mock_test_project("TEST_LONGITUDINAL")$.internal |>
    extract_project_details()
  projects <- a |> bind_rows(b) |> bind_rows(c)
  local_mocked_bindings(
    get_projects = function(...) projects
  )
  withr::local_envvar(c(
    REDCAPSYNC_TEST_DATA = "ABC123"
  ))
  expect_identical(has_envvar_token(), c(FALSE, TRUE, FALSE))
  local_mocked_bindings(
    get_projects = function(...) a
  )
  #missing from projects
  expect_null(has_envvar_token(project_names = "TEST_DATA"))
  # empty projects
  local_mocked_bindings(
    get_projects = function(...) BLANK_PROJECT_DETAILS
  )
  expect_null(has_envvar_token())
})
# is_valid_keyring (Internal)
test_that("is_valid_keyring works!", {
  skip_if_not_installed("keyring")
  # No keyring package
  local_mocked_bindings(
    has_keyring_pkg = function() FALSE
  )
  expect_false(is_valid_keyring())
  # Keyring package available, but no system support
  local_mocked_bindings(
    has_keyring_pkg = function() TRUE,
    has_keyring_support = function() FALSE
  )
  expect_false(is_valid_keyring(keyring = "keyring"))
  expect_true(is_valid_keyring())
  # Keyring specified, but locked
  local_mocked_bindings(
    has_keyring_pkg = function() TRUE,
    has_keyring_support = function() TRUE,
    keyring_is_locked = function(keyring) TRUE
  )
  expect_false(is_valid_keyring(keyring = "test_keyring"))
  # Keyring not locked, but doesn't exist
  local_mocked_bindings(
    has_keyring_pkg = function() TRUE,
    has_keyring_support = function() TRUE,
    keyring_is_locked = function(keyring) FALSE,
    keyring_list = function() {
      data.frame(keyring = "existing_keyring", stringsAsFactors = FALSE)
    }
  )
  expect_false(is_valid_keyring(keyring = "nonexistent_keyring"))
  # All good
  local_mocked_bindings(
    has_keyring_pkg = function() TRUE,
    has_keyring_support = function() TRUE,
    keyring_is_locked = function(keyring) FALSE,
    keyring_list = function() {
      data.frame(keyring = "test_keyring", stringsAsFactors = FALSE)
    }
  )
  expect_true(is_valid_keyring(keyring = "test_keyring"))
})
