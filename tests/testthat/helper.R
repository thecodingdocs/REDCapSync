generate_hex <- function(length = 32L) {
  c(0L:9L, letters[1L:6L]) |>
    sample(length, replace = TRUE) |>
    paste(collapse = "") |>
    toupper()
}
real_dev_tokens <- c(
  REDCapSync_TEST_REDCAPR_SIMPLE = "9A068C425B1341D69E83064A2D273A70",
  REDCapSync_TEST_REDCAPR_LONGITUDINAL = "DA6F2BB23146BD5A7EA3408C1A44A556",
  REDCapSync_TEST_REDCAPR_CLIN_TRIAL = "F9CBFFF78C3D78F641BAE9623F6B7E6A"
)
real_test_project <- function(project_name = "TEST_REDCAPR_SIMPLE") {
  assert_choice(project_name, .test_redcapr_names)
  REDCAPSYNC_CACHE <- Sys.getenv("REDCAPSYNC_CACHE") |>
    sanitize_path() |>
    assert_directory()
  redcap_uri <- "https://redcap-dev-2.ouhsc.edu/redcap/api/"
  project <- setup_project(project_name = project_name,
                           dir_path = REDCAPSYNC_CACHE,
                           redcap_uri = redcap_uri,
                           hard_reset = TRUE)
  project
}
mock_test_project <- function(project_name = "TEST_CLASSIC",
                              ever_connected = TRUE) {
  assert_choice(project_name, .test_project_names)
  assert_logical(ever_connected)
  REDCAPSYNC_CACHE <- Sys.getenv("REDCAPSYNC_CACHE") |>
    sanitize_path() |>
    assert_directory()
  if (ever_connected) {
    project <- load_test_project(project_name)$.internal
    project$dir_path <- set_dir(REDCAPSYNC_CACHE)
    dir.create(
      path = file.path(project$dir_path, "REDCap", project_name),
      showWarnings = FALSE
    )
    project <- REDCapSync_project$new(project)
  } else {
    project <- setup_project(project_name = project_name,
                             dir_path = REDCAPSYNC_CACHE,
                             redcap_uri = "https://redcap.fake.edu/api/",
                             hard_reset = TRUE)
  }
  project
}
mock_test_calls <- function(project_name = "TEST_CLASSIC") {
  assert_choice(project_name, .test_project_names)
  file_name <- paste0(project_name, "_call_list.rds")
  call_list <- readRDS(test_path("fixtures", file_name))
  call_list
}
