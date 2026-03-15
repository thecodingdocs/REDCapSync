generate_hex <- function(length = 32L) {
  c(0L:9L, letters[1L:6L]) |>
    sample(length, replace = TRUE) |>
    paste(collapse = "") |>
    toupper()
}
generate_comments <- function(action_type = "Add",
                              record = "1",
                              field_name = "var_branching",
                              comment = "some comment") {
  text_string <- paste0(action_type,
                        " field comment (Record: ",
                        record,
                        ", Field: ",
                        field_name)
  if (!is.null(comment)) {
    text_string <- paste0(text_string, ", Comment: \"", comment, "\"")
  }
  text_string <- paste0(text_string, ")")
  text_string
}
real_test_project <- function(project_name = "TEST_REDCAPR_SIMPLE") {
  assert_choice(project_name, .test_redcapr_names)
  cache_location <- Sys.getenv("R_USER_CACHE_DIR") |>
    sanitize_path() |>
    assert_directory()
  redcap_uri <- "https://redcap-dev-2.ouhsc.edu/redcap/api/"
  project <- setup_project(project_name = project_name,
                           dir_path = cache_location,
                           redcap_uri = redcap_uri,
                           hard_reset = TRUE)
  project
}
mock_test_project <- function(project_name = "TEST_CLASSIC") {
  assert_choice(project_name, .test_project_names)
  dir_path <- Sys.getenv("R_USER_CACHE_DIR") |>
    sanitize_path() |>
    assert_directory()
  project <- load_test_project(project_name = project_name,
                               dir_path = dir_path)
  project
}
mock_test_calls <- function(project_name = "TEST_CLASSIC") {
  assert_choice(project_name, .test_project_names)
  file_name <- paste0(project_name, "_call_list.rds")
  call_list <- readRDS(test_path("fixtures", file_name))
  call_list
}
.real_dev_tokens <- c(
  REDCAPSYNC_TEST_REDCAPR_SIMPLE = "9A068C425B1341D69E83064A2D273A70",
  REDCAPSYNC_TEST_REDCAPR_LONGITUDINAL = "DA6F2BB23146BD5A7EA3408C1A44A556",
  REDCAPSYNC_TEST_REDCAPR_CLIN_TRIAL = "F9CBFFF78C3D78F641BAE9623F6B7E6A"
)
