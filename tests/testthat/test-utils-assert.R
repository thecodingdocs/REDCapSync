# assert_dir ( Internal )
test_that("assert_dir  works!", {
})
# get_project_token ( Internal )
test_that("get_project_token  works!", {
})
# assert_web_link ( Internal )
test_that("assert_web_link  works!", {
})
# assert_env_name ( Internal )
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
  expect_no_error(assert_env_name("ABCD", max.chars = 4))
  expect_error(assert_env_name("ABCDE", max.chars = 4))
  expect_error(assert_env_name("_ABCD", underscore_allowed_first = FALSE))
  expect_no_error(assert_env_name("_ABCD", underscore_allowed_first = TRUE))
})
# assert_blank_project ( Internal )
test_that("assert_blank_project  works!", {
})
# assert_setup_project ( Internal )
test_that("assert_setup_project  works!", {
})
# assert_collection ( Internal )
test_that("assert_collection  works!", {
})
# assert_project_details ( Internal )
test_that("assert_project_details  works!", {
})
# assert_project_path ( Internal )
test_that("assert_project_path  works!", {
})
