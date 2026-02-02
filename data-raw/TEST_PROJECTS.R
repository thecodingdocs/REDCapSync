## code to prepare `TEST_PROJECTS` dataset goes here
devtools::load_all()
scrub_test_project <- function(project, pid) {
  project$project_name
  project$internals$is_test <- TRUE
  project$dir_path <- "fake/path"
  project$redcap$project_info$project_id <-
    project$redcap$project_id <- pid
  project$redcap$log$username <- "u1230"
  project$redcap$log$details <- NA
  project$redcap$log <- project$redcap$log[which(!is.na(project$redcap$log$record)), ]
  project$redcap$log <- project$redcap$log[which(project$redcap$log$action_type != "Users"), ]
  project$redcap$users$username <- "u1230"
  project$redcap$users$email <- "thecodingdocs@gmail.com"
  project$links$redcap_uri <- "https://redcap.fake.edu/api/"
  project$links$redcap_base <-  project$links$redcap_uri |>
    dirname() |> paste0("/")
  project <- update_project_links(project)
  project$summary$REDCapSync_raw$dir_other <- "fake/path"
  project$summary$REDCapSync_raw$file_path <-
    paste0("fake/path/", project$project_name, "_REDCapSync.xslx")
  project$summary$REDCapSync$dir_other <- "fake/path"
  project$summary$REDCapSync$file_path <-
    paste0("fake/path/", project$project_name, ".xslx")
  invisible(project)
}
scrub_test_rcon <- function(rcon_list, pid) {
  if (is_something(rcon_list$project_info$project_id)) {
    rcon_list$project_info$project_id <- pid
  }
  if (is_something(rcon_list$logging)) {
    rcon_list$logging$username <- "u1230"
    rcon_list$logging$details <- "REDACTED"
  }
  if (is_something(rcon_list$users)) {
    rcon_list$users$username <- "u1230"
    rcon_list$users$email <- "thecodingdocs@gmail.com"
  }
  if (is_something(rcon_list$user_role_assignment)) {
    rcon_list$user_role_assignment$username <- "u1230"
  }
  rcon_list
}
entire_log <- TRUE
hard_reset <- FALSE
# now -----
project <- setup_project(
  project_name = "TEST_CLASSIC",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  entire_log = entire_log,
  hard_reset = hard_reset
)$sync()
project <- setup_project(
  project_name = "TEST_REPEATING",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
project <- setup_project(
  project_name = "TEST_CANCER",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
project <- setup_project(
  project_name = "TEST_DATA",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
project <- setup_project(
  project_name = "TEST_EDGE",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
project <- setup_project(
  project_name = "TEST_LONGITUDINAL",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
project <- setup_project(
  project_name = "TEST_MULTIARM",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
# redcapr --------------------
Sys.setenv("REDCapSync_TEST_REDCAPR_SIMPLE" = "9A068C425B1341D69E83064A2D273A70")
Sys.setenv("REDCapSync_TEST_REDCAPR_LONGITUDINAL" = "DA6F2BB23146BD5A7EA3408C1A44A556")
Sys.setenv("REDCapSync_TEST_REDCAPR_CLIN_TRIAL" = "F2A02137BA58ABFC001058ADAC9B36D2")
project <- setup_project(
  project_name = "TEST_REDCAPR_SIMPLE",
  redcap_uri = "https://redcap-dev-2.ouhsc.edu/redcap/api/",
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
project <- setup_project(
  project_name = "TEST_REDCAPR_LONGITUDINAL",
  redcap_uri = "https://redcap-dev-2.ouhsc.edu/redcap/api/",
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
project <- setup_project(
  project_name = "TEST_REDCAPR_CLIN_TRIAL",
  redcap_uri = "https://redcap-dev-2.ouhsc.edu/redcap/api/",
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  hard_reset = hard_reset
)$sync()
# save ------
.test_project_names |> paste0(",\n") |> cat()
project_names <- c(
  "TEST_CLASSIC",
  "TEST_REPEATING",
  "TEST_LONGITUDINAL",
  "TEST_MULTIARM",
  "TEST_DATA",
  "TEST_CANCER",
  "TEST_REDCAPR_SIMPLE",
  "TEST_REDCAPR_LONGITUDINAL",
  "TEST_REDCAPR_CLIN_TRIAL"
)
names(project_names) <- paste0("1234", seq_len(length(project_names)))
project_name <- project_names |> sample(1)
projects <- get_projects()
for (project_name in project_names) {
  pid <- names(project_names)[which(project_names == project_name)]
  project <- load_project(project_name)$.internal
  rcon_list <- rcon_result(project)
  rcon <- redcapConnection(url = project$links$redcap_uri,
                           token = get_project_token(project))
  rcon_list$redcap_version <- rcon$version()
  rcon_list$call_date <- Sys.Date()
  rcon_list$data <- get_redcap_denormalized(project = project)
  if(!startsWith(project_name,"TEST_REDCAPR_")){
    rcon_list <- scrub_test_rcon(rcon_list, pid)
    project <- project |> scrub_test_project(pid = pid)
  }
  project$internals$is_test <- TRUE
  saveRDS(
    rcon_list,
    file.path("tests/testthat/fixtures/", paste0(project_name, "_call_list.rds"))
  )
  assign(
    x = project_name,
    value = project,
    envir = globalenv()
  )
}
usethis::use_data(
  TEST_CLASSIC,
  TEST_REPEATING,
  TEST_LONGITUDINAL,
  TEST_MULTIARM,
  TEST_EDGE,
  TEST_DATA,
  TEST_CANCER,
  TEST_REDCAPR_SIMPLE,
  TEST_REDCAPR_LONGITUDINAL,
  TEST_REDCAPR_CLIN_TRIAL,
  overwrite = TRUE,
  internal = TRUE
)
# end -------
