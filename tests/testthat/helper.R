generate_hex <- function(length = 32L) {
  c(0L:9L, letters[1L:6L]) |>
    sample(length, replace = TRUE) |>
    paste(collapse = "") |>
    toupper()
}
REDCapR_project <- function(){
  setup_project(
    project_name = "TEST_REDCAPR",
    redcap_uri = "https://redcap-dev-2.ouhsc.edu/redcap/api/"
  )
}
