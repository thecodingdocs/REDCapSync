# Synchronizing REDCap Project

[R6](https://r6.r-lib.org/reference/R6Class.html) project object for
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)
This is the main class for managing REDCap data, metadata, and sync
operations. Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
To reopen an existing project, use
[`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

## Value

An R6ClassGenerator which is used internally to create or load a project
object for the user

## Details

The methods documented below are functions that work without having to
use "\<-". For example, if you load a project with
`project <- load_project("TEST_CLASSIC")`, and then then run
`project$sync()`, then the project object will contain the updated data.
That function actually invisibly returns itself which allows for
"chaining", such as `load_project("TEST_CLASSIC")$sync()`. More features
are being developed that will allow for the addition of fields (outside
of REDCap).

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.'

## Active bindings

- `project_name`:

  Read-only character string of project_name as assigned with
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

- `dir_path`:

  Read-only directory path assigned with
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

- `data`:

  Read-only named list where each name is an instrument name. See public
  methods for REDCapSyncProject.

- `metadata`:

  Read-only named list with REDCap metadata. See public methods for
  REDCapSyncProject.

- `redcap`:

  Read-only named list with REDCap information including users and log.

- `.internal`:

  Read-only internal project object for custom workflows

## Methods

### Public methods

- [`REDCapSyncProject$new()`](#method-REDCapSyncProject-initialize)

- [`REDCapSyncProject$print()`](#method-REDCapSyncProject-print)

- [`REDCapSyncProject$sync()`](#method-REDCapSyncProject-sync)

- [`REDCapSyncProject$add_dataset()`](#method-REDCapSyncProject-add_dataset)

- [`REDCapSyncProject$load_dataset()`](#method-REDCapSyncProject-load_dataset)

- [`REDCapSyncProject$remove_datasets()`](#method-REDCapSyncProject-remove_datasets)

- [`REDCapSyncProject$generate_dataset()`](#method-REDCapSyncProject-generate_dataset)

- [`REDCapSyncProject$save_datasets()`](#method-REDCapSyncProject-save_datasets)

- [`REDCapSyncProject$save_dataset()`](#method-REDCapSyncProject-save_dataset)

- [`REDCapSyncProject$save()`](#method-REDCapSyncProject-save)

- [`REDCapSyncProject$set_keyring_token()`](#method-REDCapSyncProject-set_keyring_token)

- [`REDCapSyncProject$test_token()`](#method-REDCapSyncProject-test_token)

- [`REDCapSyncProject$url_launch()`](#method-REDCapSyncProject-url_launch)

- [`REDCapSyncProject$url_record_launch()`](#method-REDCapSyncProject-url_record_launch)

- [`REDCapSyncProject$upload()`](#method-REDCapSyncProject-upload)

------------------------------------------------------------------------

### `REDCapSyncProject$new()`

Active binding are read-only

The end user will not see `project$new()`. This is handled internally.
Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
The remain methods will be accessible to any user.

#### Usage

    REDCapSyncProject$new(project)

#### Arguments

- `project`:

  a list object meant to be stored internally within R6

------------------------------------------------------------------------

### `REDCapSyncProject$print()`

Print some key project information

#### Usage

    REDCapSyncProject$print()

------------------------------------------------------------------------

### `REDCapSyncProject$sync()`

Updates the REDCap data for (`project` object) by checking REDCap log
for changes. Sync is performed according to the `sync_frequency` set in
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
by default. Use `hard_check` to force a check, or ` hard_reset` to force
a complete refresh. As a default, this object will be saved to your
directory when necessary.

#### Usage

    REDCapSyncProject$sync(
      save_datasets = TRUE,
      save_to_dir = TRUE,
      hard_check = FALSE,
      hard_reset = FALSE
    )

#### Arguments

- `save_datasets`:

  Logical (TRUE/FALSE). If TRUE, saves the datasets that were previously
  added during a sync. Default is `TRUE`.

- `save_to_dir`:

  Logical (TRUE/FALSE). If TRUE, saves the updated data in the project
  object to the directory at `dir_path`. Ignored when `dir_path` is
  `NULL`. Default is `TRUE`.

- `hard_check`:

  Will check REDCap even if not due (see `sync_frequency` parameter from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md))

- `hard_reset`:

  Logical. If `TRUE`, overwrite existing dataset files with the same
  name. Default is `FALSE`.

------------------------------------------------------------------------

### `REDCapSyncProject$add_dataset()`

Add a new dataset entry

#### Usage

    REDCapSyncProject$add_dataset(
      dataset_name,
      transformation_type = "default",
      merge_form_name = "merged",
      filter_field = NULL,
      filter_choices = NULL,
      filter_list = NULL,
      filter_strict = TRUE,
      field_names = NULL,
      form_names = NULL,
      exclude_identifiers = TRUE,
      exclude_free_text = FALSE,
      date_handling = "none",
      labelled = TRUE,
      clean = TRUE,
      drop_blanks = FALSE,
      drop_missing_codes = FALSE,
      drop_others = NULL,
      include_metadata = TRUE,
      include_records = TRUE,
      include_users = TRUE,
      include_log = FALSE,
      annotate_from_log = TRUE,
      include_comments = TRUE,
      with_links = TRUE,
      separate = FALSE,
      use_csv = FALSE,
      dir_other = NULL,
      file_name = NULL,
      hard_reset = FALSE
    )

#### Arguments

- `dataset_name`:

  Character. The name of the configured dataset from which to generate
  the dataset. \*If you provide `dataset_name` all other parameters are
  inherited according to what was set with `add_dataset`.

- `transformation_type`:

  Character scalar. How to transform data for the dataset. Default is
  "default". Other options are "none" and "merge_non_repeating".
  "default" first merges non-repeating and if there are repeating forms,
  it also merges non-repeating variables to the right. "none" does not
  transform anything. "merge_non_repeating" still merges all
  non-repeating instruments but does not merge them to repeating
  instruments.

- `merge_form_name`:

  A character string representing the name of the merged form. Default
  is "merged".

- `filter_field`:

  Character. The name of the field in the database to filter on. Used
  with `filter_choices`.

- `filter_choices`:

  Vector. The values of `filter_field` used to define the dataset. An
  alternative to providing a full `filter_list`.

- `filter_list`:

  Vector. The values of `filter_field` used to define the dataset. Names
  are field names; values are the allowed value set(s). Use either
  `filter_list` or `filter_field` with `filter_choices`.

- `filter_strict`:

  Logical. If `TRUE`, all forms will be filtered by criteria. If
  `FALSE`, will convert original filter to ID column and filter all
  other forms by that record. Default is `TRUE`.

- `field_names`:

  Character vector. Names of specific fields to include in the dataset.
  Default is `NULL`, which includes all fields.

- `form_names`:

  Character vector. Names of forms to include in the dataset. Default is
  `NULL`, which includes all forms.

- `exclude_identifiers`:

  Logical. Whether to exclude identifiers in the data in the dataset.
  Default is `TRUE`.

- `exclude_free_text`:

  Logical. If `TRUE`, exclude free text fields intended for
  deidentification workflows. Default is `FALSE`.

- `date_handling`:

  character string. One of `none`,`exclude_dates`,
  `random_shift_by_record`, `random_shift_by_project`, `zero_by_record`,
  or `zero_by_project`. Random shift is +/- 90 unless changed with
  options.

- `labelled`:

  Logical. If `TRUE`, the data will be converted to labelled. If
  `FALSE`, returns raw coded values. Default is `TRUE`.

- `clean`:

  Logical. If `TRUE`, the data will be cleaned (e.g., standardizing
  missing/blank values) before summarizing. Default is `TRUE`. If
  missing codes are present in a number or date variable, R will convert
  missing codes to NA and will make that variable not upload compatible.

- `drop_blanks`:

  Logical. If `TRUE`, records with blank fields will be dropped during
  cleaning. Default is `TRUE`.

- `drop_missing_codes`:

  Logical. If `TRUE`, will convert missing codes to NA. Default is
  `FALSE`.

- `drop_others`:

  Character vector of other values that should be dropped.

- `include_metadata`:

  Logical. If `TRUE`, metadata will be included in the dataset. Default
  is `TRUE`.

- `include_records`:

  Logical. If `TRUE`, a record dataset will be included in the generated
  dataset. Default is `TRUE`.

- `include_users`:

  Logical. If `TRUE`, user-related information will be included in the
  dataset. Default is `TRUE`.

- `include_log`:

  Logical. If `TRUE`, the log of changes will be included in the
  dataset. Default is `TRUE`.

- `annotate_from_log`:

  Logical. If `TRUE`, the metadata, users, and records will be annotated
  using the log. Default is `TRUE`.

- `include_comments`:

  Logical. If `TRUE`, the comments will be included. Default is `TRUE`.

- `with_links`:

  Optional logical (TRUE/FALSE) for including links in Excel sheets.
  Default is `FALSE`.

- `separate`:

  Optional logical (TRUE/FALSE) separating each form into separate files
  as opposed to multi-tab Excel. Default is `FALSE`.

- `use_csv`:

  Logical (TRUE/FALSE). If TRUE, uses CSV files for data storage.
  Default is `FALSE`

- `dir_other`:

  Character. The directory where the dataset file will be saved. Default
  is the `output` folder within the database directory.

- `file_name`:

  Character. The base name of the file where the dataset will be saved.
  Default is `<project_name>_<dataset_name>`.

- `hard_reset`:

  Logical. If `TRUE`, overwrite existing dataset files with the same
  name. Default is `FALSE`.

------------------------------------------------------------------------

### `REDCapSyncProject$load_dataset()`

Load dataset if previously defined with `add_dataset`.

#### Usage

    REDCapSyncProject$load_dataset(dataset_name, envir = NULL)

#### Arguments

- `dataset_name`:

  Character. The name of the configured dataset from which to generate
  the dataset. \*If you provide `dataset_name` all other parameters are
  inherited according to what was set with `add_dataset`.

- `envir`:

  environment variable such as
  [`globalenv()`](https://rdrr.io/r/base/environment.html)

------------------------------------------------------------------------

### `REDCapSyncProject$remove_datasets()`

Clear all or specified datasets from the `project` object.

#### Usage

    REDCapSyncProject$remove_datasets(dataset_names = NULL)

#### Arguments

- `dataset_names`:

  One or more dataset names. Default is `NULL`.

------------------------------------------------------------------------

### `REDCapSyncProject$generate_dataset()`

Generate
[dataset](https://thecodingdocs.github.io/REDCapSync/reference/dataset.md)
object. This is usually handled internally for some default behavior but
is provided here for ad-hoc custom datasets.

#### Usage

    REDCapSyncProject$generate_dataset(
      dataset_name,
      envir = NULL,
      transformation_type = "default",
      merge_form_name = "merged",
      filter_field = NULL,
      filter_choices = NULL,
      filter_list = NULL,
      filter_strict = TRUE,
      field_names = NULL,
      form_names = NULL,
      exclude_identifiers = TRUE,
      exclude_free_text = FALSE,
      date_handling = "none",
      labelled = TRUE,
      clean = TRUE,
      drop_blanks = FALSE,
      drop_missing_codes = FALSE,
      drop_others = NULL,
      include_metadata = TRUE,
      include_records = TRUE,
      include_users = TRUE,
      include_log = FALSE,
      annotate_from_log = TRUE,
      include_comments = FALSE
    )

#### Arguments

- `dataset_name`:

  Character. The name of the configured dataset from which to generate
  the dataset. \*If you provide `dataset_name` all other parameters are
  inherited according to what was set with `add_dataset`.

- `envir`:

  environment variable such as
  [`globalenv()`](https://rdrr.io/r/base/environment.html)

- `transformation_type`:

  Character scalar. How to transform data for the dataset. Default is
  "default". Other options are "none" and "merge_non_repeating".
  "default" first merges non-repeating and if there are repeating forms,
  it also merges non-repeating variables to the right. "none" does not
  transform anything. "merge_non_repeating" still merges all
  non-repeating instruments but does not merge them to repeating
  instruments.

- `merge_form_name`:

  A character string representing the name of the merged form. Default
  is "merged".

- `filter_field`:

  Character. The name of the field in the database to filter on. Used
  with `filter_choices`.

- `filter_choices`:

  Vector. The values of `filter_field` used to define the dataset. An
  alternative to providing a full `filter_list`.

- `filter_list`:

  Vector. The values of `filter_field` used to define the dataset. Names
  are field names; values are the allowed value set(s). Use either
  `filter_list` or `filter_field` with `filter_choices`.

- `filter_strict`:

  Logical. If `TRUE`, all forms will be filtered by criteria. If
  `FALSE`, will convert original filter to ID column and filter all
  other forms by that record. Default is `TRUE`.

- `field_names`:

  Character vector. Names of specific fields to include in the dataset.
  Default is `NULL`, which includes all fields.

- `form_names`:

  Character vector. Names of forms to include in the dataset. Default is
  `NULL`, which includes all forms.

- `exclude_identifiers`:

  Logical. Whether to exclude identifiers in the data in the dataset.
  Default is `TRUE`.

- `exclude_free_text`:

  Logical. If `TRUE`, exclude free text fields intended for
  deidentification workflows. Default is `FALSE`.

- `date_handling`:

  character string. One of `none`,`exclude_dates`,
  `random_shift_by_record`, `random_shift_by_project`, `zero_by_record`,
  or `zero_by_project`. Random shift is +/- 90 unless changed with
  options.

- `labelled`:

  Logical. If `TRUE`, the data will be converted to labelled. If
  `FALSE`, returns raw coded values. Default is `TRUE`.

- `clean`:

  Logical. If `TRUE`, the data will be cleaned (e.g., standardizing
  missing/blank values) before summarizing. Default is `TRUE`. If
  missing codes are present in a number or date variable, R will convert
  missing codes to NA and will make that variable not upload compatible.

- `drop_blanks`:

  Logical. If `TRUE`, records with blank fields will be dropped during
  cleaning. Default is `TRUE`.

- `drop_missing_codes`:

  Logical. If `TRUE`, will convert missing codes to NA. Default is
  `FALSE`.

- `drop_others`:

  Character vector of other values that should be dropped.

- `include_metadata`:

  Logical. If `TRUE`, metadata will be included in the dataset. Default
  is `TRUE`.

- `include_records`:

  Logical. If `TRUE`, a record dataset will be included in the generated
  dataset. Default is `TRUE`.

- `include_users`:

  Logical. If `TRUE`, user-related information will be included in the
  dataset. Default is `TRUE`.

- `include_log`:

  Logical. If `TRUE`, the log of changes will be included in the
  dataset. Default is `TRUE`.

- `annotate_from_log`:

  Logical. If `TRUE`, the metadata, users, and records will be annotated
  using the log. Default is `TRUE`.

- `include_comments`:

  Logical. If `TRUE`, the comments will be included. Default is `TRUE`.

------------------------------------------------------------------------

### `REDCapSyncProject$save_datasets()`

saves datasets to Excel via setting provided to `add_dataset`.

#### Usage

    REDCapSyncProject$save_datasets(hard_reset = FALSE)

#### Arguments

- `hard_reset`:

  Logical. If `TRUE`, overwrite existing dataset files with the same
  name. Default is `FALSE`.

------------------------------------------------------------------------

### `REDCapSyncProject$save_dataset()`

saves dataset to Excel via setting provided to `add_dataset`.

#### Usage

    REDCapSyncProject$save_dataset(dataset_name)

#### Arguments

- `dataset_name`:

  Character. The name of the configured dataset from which to generate
  the dataset. \*If you provide `dataset_name` all other parameters are
  inherited according to what was set with `add_dataset`.

------------------------------------------------------------------------

### `REDCapSyncProject$save()`

Save project object to directory chosen with
[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

#### Usage

    REDCapSyncProject$save()

------------------------------------------------------------------------

### `REDCapSyncProject$set_keyring_token()`

Set keyring token. See vignette and config for detail.

#### Usage

    REDCapSyncProject$set_keyring_token()

------------------------------------------------------------------------

### `REDCapSyncProject$test_token()`

test connection via communication with API.

#### Usage

    REDCapSyncProject$test_token()

------------------------------------------------------------------------

### `REDCapSyncProject$url_launch()`

opens links in browser.

#### Usage

    REDCapSyncProject$url_launch(link_type = "home", open_browser = TRUE)

#### Arguments

- `link_type`:

  Character. Type of REDCap URL to retrieve. Choose one of "base",
  "home", "record_home", "records_dashboard", "api", "api_playground",
  "codebook", "user_rights", "setup", "logging", "designer",
  "dictionary", "data_quality", or "identifiers".

- `open_browser`:

  Logical. If TRUE, launches the link in the default browser.

------------------------------------------------------------------------

### `REDCapSyncProject$url_record_launch()`

opens record links in browser.

#### Usage

    REDCapSyncProject$url_record_launch(
      record = NULL,
      page = NULL,
      instance = NULL,
      open_browser = TRUE
    )

#### Arguments

- `record`:

  character of record

- `page`:

  character of page (instrument/form)

- `instance`:

  character of instance

- `open_browser`:

  Logical. If TRUE, launches the link in the default browser.

------------------------------------------------------------------------

### `REDCapSyncProject$upload()`

This will only overwrite and new data. It will not directly delete any
data. Because this is a function that can mess up your data, use it very
carefully. Remember all changes are saved in the REDCap log if there's
an issue. Missing rows and columns are allowed!

#### Usage

    REDCapSyncProject$upload(to_be_uploaded)

#### Arguments

- `to_be_uploaded`:

  data.frame in raw coded form to upload. uploading to REDCap. Default
  is 500L.

## Examples

``` r
project <- load_project("TEST_CLASSIC")
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API
project$sync()
#> ℹ TEST projects do not communicate with the API
#> ! Failed to save dataset `REDCapSync`.
project$save()
#> ℹ TEST projects do not save to directories!
```
