# Setup or Load REDCapSync Project

Setup or Load the `project` object for pipeline.

## Usage

``` r
setup_project(
  project_name,
  dir_path,
  redcap_uri,
  token_name = paste0("REDCapSync_", project_name),
  sync_frequency = "daily",
  labelled = TRUE,
  hard_reset = FALSE,
  get_type = "identified",
  records = NA,
  fields = NA,
  forms = NA,
  events = NA,
  filter_logic = NA,
  metadata_only = FALSE,
  batch_size_download = 2000L,
  batch_size_upload = 500L,
  entire_log = FALSE,
  days_of_log = 10L,
  timezone = Sys.timezone(),
  get_files = FALSE,
  get_file_repository = FALSE,
  original_file_names = FALSE,
  add_default_fields = FALSE,
  add_default_transformation = FALSE,
  add_default_summaries = TRUE
)

load_project(project_name)

load_test_project(project_name)
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
  `REDCapSync_<project_name>`

- sync_frequency:

  Frequency of sync. Options are "always", "hourly", 'daily', 'weekly',
  "monthly",and "never". The check is only triggered by calling the
  function, but can be automated with other packages. Default is `daily`

- labelled:

  Logical. If `TRUE`, the data will be converted to labelled. Default is
  `TRUE`.

- hard_reset:

  Logical (TRUE/FALSE). If TRUE, forces the setup even if the `project`
  object already exists. Default is `FALSE`.

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

- metadata_only:

  Logical (TRUE/FALSE). If TRUE, updates only the metadata. Default is
  `FALSE`.

- batch_size_download:

  Integer. Number of records to process in each batch. Default is
  `2000`.

- batch_size_upload:

  Integer. Number of records to process in each batch. Default is `500`.

- entire_log:

  Logical (TRUE/FALSE). If TRUE, retrieves the entire log. Default is
  `FALSE`.

- days_of_log:

  Integer. Number of days to be checked in the log if a hard_reset or
  new project is setup. Default is `10`.

- timezone:

  optional timezone set of the REDCap server. Otherwise, will assume
  Sys.timezone. Options from
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html).

- get_files:

  Logical (TRUE/FALSE). If TRUE, retrieves files from REDCap. Default is
  `FALSE`.

- get_file_repository:

  Logical (TRUE/FALSE). If TRUE, retrieves file repository from REDCap.
  Default is `FALSE`.

- original_file_names:

  Logical (TRUE/FALSE). If TRUE, uses original file names for retrieved
  files. Default is `FALSE`.

- add_default_fields:

  Logical (TRUE/FALSE). If TRUE, will add default fields

- add_default_transformation:

  Logical (TRUE/FALSE). If TRUE, will add default transformation

- add_default_summaries:

  Logical (TRUE/FALSE). If TRUE, will add default summaries

## Value

REDCapSync `project` list object.

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
cannot save/load/update projects.

`load_project` can be used with just the `project_name` parameter after
you have already run `setup_project` in the past with an established
directory. `dir_path` is optional for this function but can be used if
you relocated the directory.

## See also

[`get_projects`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)
for retrieving a list of projects from the directory cache.

## Examples

``` r
if (FALSE) {
# Initialize the project object with the REDCap API token and URL
project <- setup_project(
  project_name = "TEST",
  dir_path = "path/to/secure/file/storage",
  redcap_uri = "https://redcap.yourinstitution.edu/api/"
)
project <- load_project("TEST")
}
```
