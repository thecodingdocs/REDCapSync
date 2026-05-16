# Setup or Load REDCapSync Project

Setup or load a REDCapSync project object. Prepares a new REDCapSync
project by recording the REDCap URI, token name, sync settings, and
optional data selection preferences.

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
  id_position = 1L,
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

  Character scalar. Unique uppercase project name with no spaces or
  symbols.

- dir_path:

  Optional character scalar. Directory where REDCap project files are
  stored. If omitted, the project is only available in the current R
  session and cannot be persisted to disk.

- redcap_uri:

  Character scalar. Base URL of the REDCap API endpoint.

- token_name:

  Optional character scalar. Environment variable name used to store the
  REDCap token. Defaults to `REDCAPSYNC_<project_name>`.

- sync_frequency:

  Character scalar. Allowed values are "always", "hourly", "daily",
  "weekly", "monthly", "once", and "never". This setting controls how
  often future syncs are considered due.

- labelled:

  Logical scalar. If `TRUE`, data will be preserved as labelled values.
  Default is `TRUE`.

- get_type:

  Character scalar. REDCap API export type. One of "identified",
  "deidentified", "deidentified_strict", or "deidentified_super_strict".
  Default is "identified".

- records:

  Optional character vector of record IDs to request.

- fields:

  Optional character vector of field names to request.

- forms:

  Optional character vector of form names to request.

- events:

  Optional character vector of event names to request.

- filter_logic:

  Optional character scalar. REDCap filter logic used to limit returned
  records or record-events.

- id_position:

  Integer scalar of the variable that uniquely identifies the subject
  (typically record_id). This defaults to the first variable in the data
  dictionary.

- get_users:

  Logical scalar. If `TRUE`, the project will be configured to retrieve
  REDCap users during sync.

- get_data:

  Logical scalar. If `TRUE`, the project will be configured to retrieve
  REDCap data during sync.

- batch_size_download:

  Integer scalar. Number of records to request per download batch.
  Default is `1000`.

- batch_size_upload:

  Integer scalar. Number of records to process per upload batch. Default
  is `500`.

- get_entire_log:

  Logical scalar. If `TRUE`, REDCap activity logs are retrieved in full.
  Default is `FALSE`.

- log_days:

  Integer scalar. Number of days of log history to consider when a new
  project is being set up or a hard reset occurs. Default is `10`.

- log_drop_details:

  Logical scalar. If `TRUE`, the log details are excluded.

- log_drop_exports:

  Logical scalar. If `TRUE`, the log export events are excluded.

- get_files:

  Logical scalar. If `TRUE`, file attachments are configured to be
  retrieved from REDCap. Default is `FALSE`.

- get_file_repository:

  Logical scalar. If `TRUE`, the file repository is configured to be
  retrieved from REDCap. Default is `FALSE`.

- original_file_names:

  Logical scalar. If `TRUE`, retrieved files keep their original names.
  Default is `FALSE`.

- add_default_datasets:

  Logical scalar. If `TRUE`, dataset templates are added to a new
  project. Default is `TRUE`.

- timezone:

  Optional character scalar. Time zone used for REDCap data timestamps.
  Defaults to [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html).

- hard_reset:

  Logical scalar. If `TRUE`, any existing project with the same
  `project_name` is ignored and a fresh project object is initialized.
  Default is `FALSE`.

## Value

R6 project object with
[REDCapSyncProject](https://thecodingdocs.github.io/REDCapSync/reference/project.md)
class.

## Details

Unless `hard_reset = TRUE`, it will first attempt to load an existing
project from cache or the supplied `dir_path` before creating a new
project object. If settings differ from the loaded project, they will be
used or it will trigger a hard reset with a warning. `setup_project()`
itself does not perform a full REDCap sync. It configures the project so
that subsequent sync actions may retrieve data, users, files, and logs
according to the project settings.

## See also

[`projects`](https://thecodingdocs.github.io/REDCapSync/reference/projects.md)
for accessing previously setup projects

## Examples

``` r
# Initialize the project object with the REDCap API token and URL
save_folder <- tempdir() # replace with real folder
project <- setup_project(
  project_name = "FIRST_PROJECT",
  dir_path = save_folder,
  redcap_uri = "https://redcap.yourinstitution.edu/api/"
)
#> ! No cached projects... use `setup_project(...)`
#> ! Setup blank project. Unable to find, load, or repair.
#> Warning: Selecting ‘env’ backend. Secrets are stored in environment variables
#> ! No valid token in session: Sys.getenv('REDCAPSYNC_FIRST_PROJECT')
```
