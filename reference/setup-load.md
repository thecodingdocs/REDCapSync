# Setup or Load REDCapSync Project

Setup or Load the `project` object for pipeline.

## Usage

``` r
setup_project(
  project_name,
  dir_path,
  redcap_uri,
  token_name = paste0("REDCAPSYNC_", project_name),
  sync_frequency = "daily",
  labelled = TRUE,
  get_type = "identified",
  records = NA,
  fields = NA,
  forms = NA,
  events = NA,
  filter_logic = NA,
  get_users = TRUE,
  get_data = TRUE,
  batch_size_download = 1000L,
  batch_size_upload = 500L,
  get_entire_log = FALSE,
  log_days = 10L,
  log_drop_details = FALSE,
  log_drop_exports = FALSE,
  get_files = FALSE,
  get_file_repository = FALSE,
  original_file_names = FALSE,
  add_default_datasets = TRUE,
  timezone = Sys.timezone(),
  hard_reset = FALSE
)

load_project(project_name)
```

## Arguments

- project_name:

  A character string with no spaces or symbols representing the unique
  short name for the REDCap project.

- dir_path:

  Optional character string representing the directory path where you
  want the REDCap project data to be stored. If missing, project object
  will only be in current R session.

- redcap_uri:

  A character string representing the base URL of the REDCap server.

- token_name:

  An optional character string for setting your token name. Default is
  `REDCAPSYNC_<project_name>`

- sync_frequency:

  Frequency of sync. Options are "always", "hourly", 'daily', 'weekly',
  "monthly",and "never". The check is only triggered by calling the
  function, but can be automated with other packages. Default is `daily`

- labelled:

  Logical. If `TRUE`, the data will be converted to labelled. Default is
  `TRUE`.

- get_type:

  optional character of REDCap API call type. data as if user ran
  `sync_project`. Default is `FALSE`.

- records:

  An array, where each element corresponds to the ID of a desired
  record. Optional.

- fields:

  An array, where each element corresponds to a desired project field.
  Optional.

- forms:

  An array, where each element corresponds to a desired project form.
  Optional.

- events:

  An array, where each element corresponds to a desired project event.
  Optional.

- filter_logic:

  String of logic text (e.g., `[gender] = 'male'`) for filtering the
  data to be returned by this API method, in which the API will only
  return the records (or record-events, if a longitudinal project) where
  the logic evaluates as TRUE. An blank/empty string returns all
  records.

- get_users:

  Logical (TRUE/FALSE). If TRUE, will get REDCap users

- get_data:

  Logical (TRUE/FALSE). If TRUE, will get REDCap data.

- batch_size_download:

  Integer. Number of records to process in each batch. Default is
  `1000`.

- batch_size_upload:

  Integer. Number of records to process in each batch. Default is `500`.

- get_entire_log:

  Logical (TRUE/FALSE). If TRUE, retrieves the entire log. Default is
  `FALSE`.

- log_days:

  Integer. Number of days to be checked in the log if a hard_reset or
  new project is setup. Default is `10`.

- log_drop_details:

  Logical (TRUE/FALSE). If TRUE, drops log details.

- log_drop_exports:

  Logical (TRUE/FALSE). If TRUE, drops log exports.

- get_files:

  Logical (TRUE/FALSE). If TRUE, retrieves files from REDCap. Default is
  `FALSE`.

- get_file_repository:

  Logical (TRUE/FALSE). If TRUE, retrieves file repository from REDCap.
  Default is `FALSE`.

- original_file_names:

  Logical (TRUE/FALSE). If TRUE, uses original file names for retrieved
  files. Default is `FALSE`.

- add_default_datasets:

  Logical (TRUE/FALSE). If TRUE, will add default datasets

- timezone:

  optional timezone set of the REDCap server. Otherwise, will assume
  Sys.timezone. Options from
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html).

- hard_reset:

  Logical (TRUE/FALSE). If TRUE, forces the setup even if the `project`
  object already exists. Default is `FALSE`.

## Value

R6 project object with
[REDCapSyncProject](https://thecodingdocs.github.io/REDCapSync/reference/project.md)
class.

## Details

This function sets up the `project` object by storing the REDCap API
token and other configurations required for interacting with the REDCap
server. It ensures that the token is valid and ready for use in
subsequent API calls. Neither function directly attempts communication
with REDCap.

`setup_project` is used the first time you initialize/link a REDCap
project. Mainly, it sets your unique `project_name` and your intended
directory. Unless you run `hard_reset = TRUE` the default will first try
load_project. dir_path is technically optional but without it the user
cannot save/load/update projects. Must be all capital letters!

`load_project` can be used with just the `project_name` parameter after
you have already run `setup_project` in the past with an established
directory. `dir_path` is optional for this function but can be used if
you relocated the directory.

## See also

[`projects`](https://thecodingdocs.github.io/REDCapSync/reference/projects.md)
for accessing previously setup projects

## Examples

``` r
# Initialize the project object with the REDCap API token and URL
save_folder <- tempdir() # replace with real folder
project <- setup_project(
  project_name = "TEST",
  dir_path = save_folder,
  redcap_uri = "https://redcap.yourinstitution.edu/api/"
)
#> ! No cached projects... use `setup_project(...)`
#> ! Setup blank project. Unable to find, load, or repair.
#> Warning: Selecting ‘env’ backend. Secrets are stored in environment variables
#> ! No valid token in session: Sys.getenv('REDCAPSYNC_TEST')
```
