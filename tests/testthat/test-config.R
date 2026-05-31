tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
# config (Exported)
# config_get (Internal)
test_that("config_get works!", {
  # defaults
  option_list <- list(
    redcapsync.config.allow.test.names = NULL,
    redcapsync.config.show.api.messages = NULL,
    redcapsync.config.verbose = NULL,
    redcapsync.config.offline = NULL,
    redcapsync.config.cache.dir = NULL,
    redcapsync.config.keyring = NULL,
    redcapsync.config.keyring.service = NULL,
    redcapsync.config.xlsx.header.color = NULL,
    redcapsync.config.xlsx.header.font.size = NULL,
    redcapsync.config.xlsx.header.font.color = NULL,
    redcapsync.config.xlsx.body.font.size = NULL,
    redcapsync.config.xlsx.body.font.color = NULL,
    redcapsync.config.xlsx.font.name = NULL
  )
  envvar_list <- list(
    R_USER_CACHE_DIR = NA,
    REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES = NA,
    REDCAPSYNC_CONFIG_SHOW_API_MESSAGES = NA,
    REDCAPSYNC_CONFIG_VERBOSE = NA,
    REDCAPSYNC_CONFIG_OFFLINE = NA,
    REDCAPSYNC_CONFIG_CACHE_DIR = NA,
    REDCAPSYNC_CONFIG_KEYRING = NA,
    REDCAPSYNC_CONFIG_KEYRING_SERVICE = NA,
    REDCAPSYNC_CONFIG_XLSX_HEADER_COLOR = NA,
    REDCAPSYNC_CONFIG_XLSX_HEADER_FONT_SIZE = NA,
    REDCAPSYNC_CONFIG_XLSX_HEADER_FONT_COLOR = NA,
    REDCAPSYNC_CONFIG_XLSX_BODY_FONT_SIZE = NA,
    REDCAPSYNC_CONFIG_XLSX_BODY_FONT_COLOR = NA,
    REDCAPSYNC_CONFIG_XLSX_FONT_NAME = NA
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
  value <- config_get(opt_name = "offline",
                      type = "logical",
                      default = FALSE)
  expect_false(value)
  value <- config_get(opt_name = "keyring",
                      type = "character",
                      default = NULL)
  expect_null(value)
  value <- config_get(opt_name = "keyring.service",
                      type = "character",
                      default = "R-REDCapSync")
  expect_identical(value, "R-REDCapSync")
  value <- config_get(opt_name = "cache.dir",
                      type = "filepath",
                      default = cache_path_default())
  expect_identical(value, cache_path_default())
  value <- config_get(opt_name = "xlsx.header.color",
                      type = "character",
                      default = "#74DFFF")
  expect_identical(value, "#74DFFF")
  value <- config_get(opt_name = "xlsx.header.font.size",
                      type = "integer",
                      default = 14L)
  expect_identical(value, 14L)
  value <- config_get(opt_name = "xlsx.header.font.color",
                      type = "character",
                      default = "black")
  expect_identical(value, "black")
  value <- config_get(opt_name = "xlsx.body.font.size",
                      type = "integer",
                      default = 11L)
  expect_identical(value, 11L)
  value <- config_get(opt_name = "xlsx.body.font.color",
                      type = "character",
                      default = "black")
  expect_identical(value, "black")
  value <- config_get(opt_name = "xlsx.font.name",
                      type = "character",
                      default = "Arial")
  expect_identical(value, "Arial")
  # envvar
  tempdir_test <- sanitize_path(withr::local_tempdir())
  envvar_list <- list(
    R_USER_CACHE_DIR = NA,
    REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES = "True",
    REDCAPSYNC_CONFIG_SHOW_API_MESSAGES = "true",
    REDCAPSYNC_CONFIG_VERBOSE = "FALSE",
    REDCAPSYNC_CONFIG_OFFLINE = TRUE,
    REDCAPSYNC_CONFIG_CACHE_DIR = tempdir_test,
    REDCAPSYNC_CONFIG_KEYRING = "REDCapSync",
    REDCAPSYNC_CONFIG_KEYRING_SERVICE = "R-REDCapSync-new",
    REDCAPSYNC_CONFIG_XLSX_HEADER_COLOR = "red",
    REDCAPSYNC_CONFIG_XLSX_HEADER_FONT_SIZE = 12L,
    REDCAPSYNC_CONFIG_XLSX_HEADER_FONT_COLOR = "white",
    REDCAPSYNC_CONFIG_XLSX_BODY_FONT_SIZE = 8L,
    REDCAPSYNC_CONFIG_XLSX_BODY_FONT_COLOR = "red",
    REDCAPSYNC_CONFIG_XLSX_FONT_NAME = "Georgia"
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
  value <- config_get(opt_name = "offline",
                      type = "logical",
                      default = FALSE)
  expect_true(value)
  value <- config_get(opt_name = "cache.dir",
                      type = "filepath",
                      default = cache_path_default())
  expect_identical(value, tempdir_test)
  value <- config_get(opt_name = "keyring",
                      type = "character",
                      default = NULL)
  expect_identical(value, "REDCapSync")
  value <- config_get(opt_name = "keyring.service",
                      type = "character",
                      default = "R-REDCapSync")
  expect_identical(value, "R-REDCapSync-new")
  value <- config_get(opt_name = "xlsx.header.color",
                      type = "character",
                      default = "#74DFFF")
  expect_identical(value, "red")
  value <- config_get(opt_name = "xlsx.header.font.size",
                      type = "integer",
                      default = 14L)
  expect_identical(value, 12L)
  value <- config_get(opt_name = "xlsx.header.font.color",
                      type = "character",
                      default = "black")
  expect_identical(value, "white")
  value <- config_get(opt_name = "xlsx.body.font.size",
                      type = "integer",
                      default = 11L)
  expect_identical(value, 8L)
  value <- config_get(opt_name = "xlsx.body.font.color",
                      type = "character",
                      default = "black")
  expect_identical(value, "red")
  value <- config_get(opt_name = "xlsx.font.name",
                      type = "character",
                      default = "Arial")
  expect_identical(value, "Georgia")
  #options override envvar
  tempdir_test2 <- sanitize_path(withr::local_tempdir())
  option_list <- list(
    redcapsync.config.allow.test.names = FALSE,
    redcapsync.config.show.api.messages = "FALSe",
    redcapsync.config.verbose = TRUE,
    redcapsync.config.offline = "false",
    redcapsync.config.cache.dir = tempdir_test2,
    redcapsync.config.keyring = "new-keyring",
    redcapsync.config.keyring.service = "R-REDCapSync",
    redcapsync.config.xlsx.header.color = "orange",
    redcapsync.config.xlsx.header.font.size = 15L,
    redcapsync.config.xlsx.header.font.color = "gray",
    redcapsync.config.xlsx.body.font.size = 6L,
    redcapsync.config.xlsx.body.font.color = "purple",
    redcapsync.config.xlsx.font.name = "Times"
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
  value <- config_get(opt_name = "offline",
                      type = "logical",
                      default = FALSE)
  expect_false(value)
  value <- config_get(opt_name = "cache.dir",
                      type = "filepath",
                      default = cache_path_default())
  expect_identical(value, tempdir_test2)
  value <- config_get(opt_name = "keyring",
                      type = "character",
                      default = NULL)
  expect_identical(value, "new-keyring")
  value <- config_get(opt_name = "keyring.service",
                      type = "character",
                      default = "R-REDCapSync")
  expect_identical(value, "R-REDCapSync")
  value <- config_get(opt_name = "xlsx.header.color",
                      type = "character",
                      default = "#74DFFF")
  expect_identical(value, "orange")
  value <- config_get(opt_name = "xlsx.header.font.size",
                      type = "integer",
                      default = 14L)
  expect_identical(value, 15L)
  value <- config_get(opt_name = "xlsx.header.font.color",
                      type = "character",
                      default = "black")
  expect_identical(value, "gray")
  value <- config_get(opt_name = "xlsx.body.font.size",
                      type = "integer",
                      default = 11L)
  expect_identical(value, 6L)
  value <- config_get(opt_name = "xlsx.body.font.color",
                      type = "character",
                      default = "black")
  expect_identical(value, "purple")
  value <- config_get(opt_name = "xlsx.font.name",
                      type = "character",
                      default = "Arial")
  expect_identical(value, "Times")
})
# config_validate (Internal)
test_that("config_validate works!", {
  value <- config_validate(
    opt_name = "allow.test.names",
    value = mtcars, # not logical
    type = "logical",
    default = TRUE
  )
  expect_true(value)
  value <- config_validate(
    opt_name = "allow.test.names",
    value = "FALSE", # character ok
    type = "logical",
    default = FALSE
  )
  expect_false(value)
  value <- config_validate(
    opt_name = "allow.test.names",
    value = TRUE,
    type = "logical",
    default = FALSE
  )
  expect_true(value)
})
