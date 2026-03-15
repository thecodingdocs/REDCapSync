tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
# config (Internal)
test_that("config works!", {
})
# config_get (Internal)
test_that("config_get works!", {
  # defaults
  option_list <- list(
    redcapsync.config.allow.test.names = NULL,
    redcapsync.config.show.api.messages = NULL,
    redcapsync.config.verbose = NULL,
    redcapsync.config.cache.dir = NULL,
    redcapsync.config.header.style = NULL,
    redcapsync.config.body.style = NULL
  )
  envvar_list <- list(
    R_USER_CACHE_DIR = NA,
    REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES = NA,
    REDCAPSYNC_CONFIG_SHOW_API_MESSAGES = NA,
    REDCAPSYNC_CONFIG_VERBOSE = NA,
    REDCAPSYNC_CONFIG_CACHE_DIR = NA,
    REDCAPSYNC_CONFIG_HEADER_STYLE = NA,
    REDCAPSYNC_CONFIG_BODY_STYLE = NA
  )
  withr::local_options(option_list)
  withr::local_envvar(envvar_list)
  value <- config_get(opt_name = "allow.test.names",
                      type = "logical",
                      default = FALSE)
  expect_false(value)
  value <- config_get(opt_name = "show.api.messages",
                      type = "logical",
                      default = FALSE)
  expect_false(value)
  value <- config_get(opt_name = "verbose",
                      type = "logical",
                      default = TRUE)
  expect_true(value)
  value <- config_get(opt_name = "cache.dir",
                      type = "filepath",
                      default = cache_path_default())
  expect_identical(value, cache_path_default())
  value <- config_get(opt_name = "header.style",
                      type = "openxlsx_style",
                      default = .header_style)
  expect_identical(value, .header_style)
  value <- config_get(opt_name = "body.style",
                      type = "openxlsx_style",
                      default = .body_style)
  expect_identical(value, .body_style)
  # envvar
  tempdir_test <- sanitize_path(withr::local_tempdir())
  envvar_list <- list(
    R_USER_CACHE_DIR = NA,
    REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES = "True",
    REDCAPSYNC_CONFIG_SHOW_API_MESSAGES = "true",
    REDCAPSYNC_CONFIG_VERBOSE = "FALSE",
    REDCAPSYNC_CONFIG_CACHE_DIR = tempdir_test,
    REDCAPSYNC_CONFIG_HEADER_STYLE = "ignored",
    REDCAPSYNC_CONFIG_BODY_STYLE = "ignored"
  )
  withr::local_envvar(envvar_list)
  value <- config_get(opt_name = "allow.test.names",
                      type = "logical",
                      default = FALSE)
  expect_true(value)
  value <- config_get(opt_name = "show.api.messages",
                      type = "logical",
                      default = FALSE)
  expect_true(value)
  value <- config_get(opt_name = "verbose",
                      type = "logical",
                      default = TRUE)
  expect_false(value)
  value <- config_get(opt_name = "cache.dir",
                      type = "filepath",
                      default = cache_path_default())
  expect_identical(value, tempdir_test)
  value <- config_get(opt_name = "header.style",
                      type = "openxlsx_style",
                      default = .header_style)
  expect_identical(value, .header_style)
  value <- config_get(opt_name = "body.style",
                      type = "openxlsx_style",
                      default = .body_style)
  expect_identical(value, .body_style)
  #options override envvar
  tempdir_test2 <- sanitize_path(withr::local_tempdir())
  option_list <- list(
    redcapsync.config.allow.test.names = FALSE,
    redcapsync.config.show.api.messages = "FALSe",
    redcapsync.config.verbose = TRUE,
    redcapsync.config.cache.dir = tempdir_test2,
    redcapsync.config.header.style = .body_style, # flipped
    redcapsync.config.body.style = .header_style # flipped
  )
  withr::local_options(option_list)
  value <- config_get(opt_name = "allow.test.names",
                      type = "logical",
                      default = FALSE)
  expect_false(value)
  value <- config_get(opt_name = "show.api.messages",
                      type = "logical",
                      default = FALSE)
  expect_false(value)
  value <- config_get(opt_name = "verbose",
                      type = "logical",
                      default = TRUE)
  expect_true(value)
  value <- config_get(opt_name = "cache.dir",
                      type = "filepath",
                      default = cache_path_default())
  expect_identical(value, tempdir_test2)
  value <- config_get(opt_name = "header.style",
                      type = "openxlsx_style",
                      default = .header_style)
  expect_identical(value, .body_style) # flipped
  value <- config_get(opt_name = "body.style",
                      type = "openxlsx_style",
                      default = .body_style)
  expect_identical(value, .header_style) # flipped
})
# config_validate (Internal)
test_that("config_validate works!", {
  # config_validate(opt_name, value, type, default) {
})
