tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(R_USER_CACHE_DIR = tempdir_file)
# assert_blank_project (Internal)
test_that("assert_blank_project works!", {
})
# assert_dir (Internal)
test_that("assert_dir works!", {
})
# assert_env_name (Internal)
test_that("assert_env_name works!", {
  expect_no_error(assert_env_name("REDCapSync"))
  expect_no_error(assert_env_name("REDCap_Sync"))
  expect_no_error(assert_env_name("my_form"))
  expect_no_error(assert_env_name("a"))
  expect_no_error(assert_env_name("A"))
  expect_error(assert_env_name("1A"))
  expect_error(assert_env_name("My Form"))
  expect_error(assert_env_name("my-form"))
  expect_error(assert_env_name("varName!"))
  expect_error(assert_env_name("_form"))
  expect_no_error(assert_env_name("ABCD", max.chars = 4L))
  expect_error(assert_env_name("ABCDE", max.chars = 4L))
})
# assert_project_details (Internal)
test_that("assert_project_details works!", {
})
# assert_project_path (Internal)
test_that("assert_project_path works!", {
})
# assert_setup_project (Internal)
test_that("assert_setup_project works!", {
})
# assert_web_link (Internal)
test_that("assert_web_link works!", {
})
# test_env_name (Internal)
test_that("test_env_name works!", {
})
# test_project_details (Internal)
test_that("test_project_details works!", {
})
# test_setup_project (Internal)
test_that("test_setup_project works!", {
})
# test_unique_character (Internal)
test_that("test_unique_character works!", {
})
