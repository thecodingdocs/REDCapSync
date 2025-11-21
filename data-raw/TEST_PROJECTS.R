## code to prepare `TEST_PROJECTS` dataset goes here
devtools::load_all()
project <- setup_project(
  short_name = "TEST_CANCER",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST")
) %>% sync_project(hard_reset = TRUE)
project <- setup_project(
  short_name = "TEST_CLASSIC",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST"),
  entire_log = TRUE
) %>% sync_project(hard_reset = TRUE)
project <- setup_project(
  short_name = "TEST_DATA",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST")
) %>% sync_project(hard_reset = TRUE)
project <- setup_project(
  short_name = "TEST_EDGE",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST")
) %>% sync_project(hard_reset = TRUE)
project <- setup_project(
  short_name = "TEST_LONGITUDINAL",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST")
) %>% sync_project(hard_reset = TRUE)
project <- setup_project(
  short_name = "TEST_MULTIARM",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST")
) %>% sync_project(hard_reset = TRUE)
project <- setup_project(
  short_name = "TEST_REPEATING",
  redcap_uri = Sys.getenv("UT_REDCAP_URI"),
  dir_path =  Sys.getenv("dir_path_UTTEST")
) %>% sync_project(hard_reset = TRUE)
scrub_test_project <- function(project){
  project$short_name
  project$internals$is_test <- TRUE
  project$dir_path <- "fake/path"
  project$redcap$project_info$project_id <-
    project$redcap$project_id <- "12340"
  project$redcap$log$username <- "u1230"
  project$redcap$log$details <- NA
  project$redcap$log <- project$redcap$log[which(!is.na(project$redcap$log$record)),]
  project$redcap$log <- project$redcap$log[which(project$redcap$log$action_type != "Users"),]
  project$redcap$users$username <- "u1230"
  project$redcap$users$email <- "thecodingdocs@gmail.com"
  project$links$redcap_uri <- "https://redcap.fake.edu/api/"
  project$links$redcap_base <-  project$links$redcap_uri %>% dirname() %>% paste0("/")
  project <- update_project_links(project)
  project$summary$REDCapSync_raw$dir_other <- "fake/path"
  project$summary$REDCapSync_raw$file_path <- paste0("fake/path/",project$short_name,"_REDCapSync.xslx")
  project$summary$REDCapSync$dir_other <- "fake/path"
  project$summary$REDCapSync$file_path <- paste0("fake/path/",project$short_name,".xslx")
  invisible(project)
}
for(short_name in .test_project_names){
  assign(
    x = short_name,
    value = load_project(short_name) %>% scrub_test_project() ,
    envir = globalenv()
  )
}
.test_project_names %>% paste0(",\n") %>% cat()
usethis::use_data(
  TEST_CLASSIC,
  TEST_REPEATING,
  # TEST_LONGITUDINAL,
  # TEST_MULTIARM,
  # TEST_EDGE,
  # TEST_DATA,
  # TEST_CANCER,
  overwrite = TRUE,
  internal = TRUE
)
