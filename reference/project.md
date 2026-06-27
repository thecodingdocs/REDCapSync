# REDCapSync Project Object

[R6](https://r6.r-lib.org/reference/R6Class.html) project object for
managing REDCap data access and synchronization. The project object is
your main interface to REDCap data through the
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)
package. It stores your REDCap configuration, data, metadata, and
provides methods to sync, transform, and export your data.

## Value

An R6 `REDCapSyncProject` class generator for internal use. Users
interact with instances created by
[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
or
[load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

## Details

### Workflow

**Initialize** a project with
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md):

    project <- setup_project(
      project_name = "FIRST_PROJECT",
      redcap_uri = "https://redcap.yourinstitution.edu/api/",
      dir_path = "~/redcap_projects"
    )

**Synchronize** REDCap data into your project object using the REDCap
API and log-based change detection:

    project$sync()

**Load** previously saved REDCap project object using cache:

    project <- load_project("FIRST_PROJECT)
    projects$names() # should see "FIRST_PROJECT"

**Access** REDCap data, metadata, and users via read-only fields:

    project$data           # Named list of REDCap forms/instruments
    project$metadata       # REDCap field definitions, forms, and choices
    project$redcap$users   # REDCap user information
    project$redcap$log     # REDCap log information

    # you can bypass read-only if needed
    users <- project$redcap$users

**Transform and export** your data into clean, analysis-ready datasets
to be used in R and/or Excel:

    project$add_dataset("analysis_data", ...) # adds to project for re-use
    project$save_datasets() # can save to Excel but is also part of future syncs

**Upload** corrected or new data back to REDCap:

    project$upload(updated_data)

### Design Features

The project object uses **log-based sync**: Since REDCap maintains a
detailed change log, `project$sync()` only retrieves and updates records
that have changed since the last sync. This dramatically reduces API
calls and improves performance for large projects.

All methods use **method chaining**: Methods invisibly return the
project object (self), allowing fluent code:

    dataset <- load_project("TEST_CLASSIC")$sync()$load_dataset("REDCapSync")

### Read-Only Fields

- `project$project_name`: Project identifier (set with
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md))

- `project$dir_path`: Persistent storage directory (set with
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md))

- `project$data`: Named list of synchronized REDCap forms

- `project$metadata`: REDCap field metadata (forms, fields, choices)

- `project$redcap`: REDCap project info, users, and activity log

- `project$.internal`: Internal project object (for advanced use)

### TEST Projects

All TEST projects start with "TEST\_" and by default the average user
cannot create a project that starts with "TEST\_". They are produced
from actual server REDCap projects but never contained real data and
were scrubbed of any "real" user/log data. They are subject to change in
future versions as the package matures. Keep in mind there are many
combinations of REDCap structures to account for.

TEST projects are meant to be used for demonstration and testing
purposes. In general, try to use actual REDCap project(s) to explore the
package.

This is how to load a project with or without specifying a directory.

    # return the list of available test projects
    REDCapSync::projects$test_names()

    project <- load_project("TEST_CLASSIC")

    YOUR_DIRECTORY <- getwd()
    project <- setup_project("TEST_CLASSIC", dir_path = YOUR_DIRECTORY)
    # now can test object functions (does not use API)
    project$sync() # will also save datasets

The currently available TEST projects are as follows:

- `TEST_CLASSIC`: Classic project (nothing repeats). Has every data
  type. Contains data for survival analysis testing. Contains comments.

- `TEST_REPEATING`: Non-longitudinal project with repeating instruments.

- `TEST_LONGITUDINAL`: Longitudinal project (with events).

- `TEST_MULTIARM`: Multi-arm and longitudinal project.

- `TEST_EDGE`: Edge-case project. Meant to test things that may be
  uncommon but should be accounted for as package matures.

- `TEST_DATA`: Placeholder project meant to contain more rich test
  dataset.

- `TEST_CANCER`: Placeholder project meant to contain more rich test
  dataset.

- `TEST_REDCAPR_SIMPLE`: Borrowed with permission from
  [REDCapR](https://ouhscbbmc.github.io/REDCapR/reference/REDCapR-package.html).

- `TEST_REDCAPR_LONGITUDINAL`: Borrowed with permission from
  [REDCapR](https://ouhscbbmc.github.io/REDCapR/reference/REDCapR-package.html).

- `TEST_REDCAPR_CLIN_TRIAL`: Borrowed with permission from
  [REDCapR](https://ouhscbbmc.github.io/REDCapR/reference/REDCapR-package.html).

## See also

[`vignette("REDCapSync", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/REDCapSync.md)
[projects](https://thecodingdocs.github.io/REDCapSync/reference/projects.md)
for shortcuts of cached setup projects
[`vignette("Tokens", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Tokens.md)
[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing projects
[dataset](https://thecodingdocs.github.io/REDCapSync/reference/dataset.md)
for using the dataset objects
[`vignette("Datasets", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Datasets.md)
[`vignette("Uploads", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Uploads.md)

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

- [`REDCapSyncProject$add_field()`](#method-REDCapSyncProject-add_field)

- [`REDCapSyncProject$remove_added_fields()`](#method-REDCapSyncProject-remove_added_fields)

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

  Logical. Save dataset outputs during sync. Default is `TRUE`.

- `save_to_dir`:

  Logical. Save updated project object to directory. Default is `TRUE`.

- `hard_check`:

  Logical. Force REDCap API check regardless of `sync_frequency`.
  Default is `FALSE`.

- `hard_reset`:

  Logical. Overwrite existing dataset files. Default is `FALSE`.

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
      exclude_identifiers = FALSE,
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
      include_added_fields = TRUE,
      with_links = TRUE,
      separate = FALSE,
      use_csv = FALSE,
      dir_other = NULL,
      file_name = NULL,
      hard_reset = FALSE
    )

#### Arguments

- `dataset_name`:

  Character. Name of the dataset configuration to create, load, or
  reference.

- `transformation_type`:

  Character. How to transform data: "default" (merge non-repeating then
  add to repeating), "none" (no transformation), or
  "merge_non_repeating" (merge non-repeating only). Default is
  "default".

- `merge_form_name`:

  Character. Name for the merged non-repeating form. Default is
  "merged".

- `filter_field`:

  Character. Field name to filter dataset on.

- `filter_choices`:

  Vector. Allowed values for `filter_field`.

- `filter_list`:

  List. Named list where names are field names and values are allowed
  value sets. Alternative to `filter_field`/`filter_choices`.

- `filter_strict`:

  Logical. If `TRUE`, filter all forms. If `FALSE`, apply filter only to
  records (ID column). Default is `TRUE`.

- `field_names`:

  Character vector. Fields to include. Default `NULL` includes all
  fields.

- `form_names`:

  Character vector. Forms to include. Default `NULL` includes all forms.

- `exclude_identifiers`:

  Logical. Exclude identifier fields. Default is `TRUE`.

- `exclude_free_text`:

  Logical. Exclude free-text fields (for deidentification). Default is
  `FALSE`.

- `date_handling`:

  Character. Date handling strategy: "none", "exclude_dates",
  "random_shift_by_record", "random_shift_by_project", "zero_by_record",
  or "zero_by_project". Default is "none".

- `labelled`:

  Logical. Convert to labelled data if `TRUE`. Default is `TRUE`.

- `clean`:

  Logical. Clean data (standardize missing values). Default is `TRUE`.

- `drop_blanks`:

  Logical. Drop records with blank fields. Default is `TRUE`.

- `drop_missing_codes`:

  Logical. Convert REDCap missing codes to `NA`. Default is `FALSE`.

- `drop_others`:

  Character vector of additional values to drop.

- `include_metadata`:

  Logical. Include field metadata in dataset. Default is `TRUE`.

- `include_records`:

  Logical. Include record-level information. Default is `TRUE`.

- `include_users`:

  Logical. Include user information. Default is `TRUE`.

- `include_log`:

  Logical. Include REDCap activity log. Default is `FALSE`.

- `annotate_from_log`:

  Logical. Annotate data using the REDCap log. Default is `TRUE`.

- `include_comments`:

  Logical. Include field comments. Default is `TRUE`.

- `include_added_fields`:

  Logical. Include added fields. Default is `TRUE`.

- `with_links`:

  Logical. Include hyperlinks in Excel exports. Default is `FALSE`.

- `separate`:

  Logical. Separate each form into distinct files (vs. multi-tab Excel).
  Default is `FALSE`.

- `use_csv`:

  Logical. Use CSV format instead of Excel. Default is `FALSE`.

- `dir_other`:

  Character. Output directory (default is project's `output` folder).

- `file_name`:

  Character. Dataset file name (default is
  `<project_name>_<dataset_name>`).

- `hard_reset`:

  Logical. Overwrite existing dataset files. Default is `FALSE`.

------------------------------------------------------------------------

### `REDCapSyncProject$add_field()`

Add or modify a field. This can be used to add derived fields for R,
Excel, and applications. It can also be used to recalculate fields for
pipelines that modify data in REDCap using R.

#### Usage

    REDCapSyncProject$add_field(
      field_name,
      form_name,
      field_type_r = "character",
      field_label = NA,
      field_choices = NA,
      field_note = NA,
      identifier = "",
      units = NA,
      data_func = NA
    )

#### Arguments

- `field_name`:

  Character. Field name to be used for added field.

- `form_name`:

  Character. Form name to be used for added field. Must be an existing
  REDCap form/instrument.

- `field_type_r`:

  Character. One of "character", "factor", "date", "datetime",
  "integer", or "numeric" . Default is `character`.

- `field_label`:

  Character. Field label for added field.

- `field_choices`:

  Character vector. Choices if `field_type_r` is "factor".

- `field_note`:

  Character Field note for added field.

- `identifier`:

  Character. Either "", or "y" per REDCap data dictionary.

- `units`:

  Character. To be used for plots and tables.

- `data_func`:

  Function. Must have "project" as the only parameter. Must return a
  vector of the field (same length and order as form). Example,
  `data_func = function(project) {...}`.

------------------------------------------------------------------------

### `REDCapSyncProject$remove_added_fields()`

Remove all added fields.

#### Usage

    REDCapSyncProject$remove_added_fields()

------------------------------------------------------------------------

### `REDCapSyncProject$load_dataset()`

Load dataset if previously defined with `add_dataset`.

#### Usage

    REDCapSyncProject$load_dataset(dataset_name, envir = NULL)

#### Arguments

- `dataset_name`:

  Character. Name of the dataset configuration to create, load, or
  reference.

- `envir`:

  Environment to assign dataset objects. Default is `NULL`.

------------------------------------------------------------------------

### `REDCapSyncProject$remove_datasets()`

Clear all or specified datasets from the `project` object.

#### Usage

    REDCapSyncProject$remove_datasets(dataset_names = NULL)

#### Arguments

- `dataset_names`:

  Character vector. Dataset names to remove/operate on. Default is
  `NULL`.

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
      exclude_identifiers = FALSE,
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
      include_comments = FALSE,
      include_added_fields = TRUE
    )

#### Arguments

- `dataset_name`:

  Character. Name of the dataset configuration to create, load, or
  reference.

- `envir`:

  Environment to assign dataset objects. Default is `NULL`.

- `transformation_type`:

  Character. How to transform data: "default" (merge non-repeating then
  add to repeating), "none" (no transformation), or
  "merge_non_repeating" (merge non-repeating only). Default is
  "default".

- `merge_form_name`:

  Character. Name for the merged non-repeating form. Default is
  "merged".

- `filter_field`:

  Character. Field name to filter dataset on.

- `filter_choices`:

  Vector. Allowed values for `filter_field`.

- `filter_list`:

  List. Named list where names are field names and values are allowed
  value sets. Alternative to `filter_field`/`filter_choices`.

- `filter_strict`:

  Logical. If `TRUE`, filter all forms. If `FALSE`, apply filter only to
  records (ID column). Default is `TRUE`.

- `field_names`:

  Character vector. Fields to include. Default `NULL` includes all
  fields.

- `form_names`:

  Character vector. Forms to include. Default `NULL` includes all forms.

- `exclude_identifiers`:

  Logical. Exclude identifier fields. Default is `TRUE`.

- `exclude_free_text`:

  Logical. Exclude free-text fields (for deidentification). Default is
  `FALSE`.

- `date_handling`:

  Character. Date handling strategy: "none", "exclude_dates",
  "random_shift_by_record", "random_shift_by_project", "zero_by_record",
  or "zero_by_project". Default is "none".

- `labelled`:

  Logical. Convert to labelled data if `TRUE`. Default is `TRUE`.

- `clean`:

  Logical. Clean data (standardize missing values). Default is `TRUE`.

- `drop_blanks`:

  Logical. Drop records with blank fields. Default is `TRUE`.

- `drop_missing_codes`:

  Logical. Convert REDCap missing codes to `NA`. Default is `FALSE`.

- `drop_others`:

  Character vector of additional values to drop.

- `include_metadata`:

  Logical. Include field metadata in dataset. Default is `TRUE`.

- `include_records`:

  Logical. Include record-level information. Default is `TRUE`.

- `include_users`:

  Logical. Include user information. Default is `TRUE`.

- `include_log`:

  Logical. Include REDCap activity log. Default is `FALSE`.

- `annotate_from_log`:

  Logical. Annotate data using the REDCap log. Default is `TRUE`.

- `include_comments`:

  Logical. Include field comments. Default is `TRUE`.

- `include_added_fields`:

  Logical. Include added fields. Default is `TRUE`.

------------------------------------------------------------------------

### `REDCapSyncProject$save_datasets()`

saves datasets to Excel via setting provided to `add_dataset`.

#### Usage

    REDCapSyncProject$save_datasets(hard_reset = FALSE)

#### Arguments

- `hard_reset`:

  Logical. Overwrite existing dataset files. Default is `FALSE`.

------------------------------------------------------------------------

### `REDCapSyncProject$save_dataset()`

saves dataset to Excel via setting provided to `add_dataset`.

#### Usage

    REDCapSyncProject$save_dataset(dataset_name)

#### Arguments

- `dataset_name`:

  Character. Name of the dataset configuration to create, load, or
  reference.

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

    REDCapSyncProject$test_token(keyring_if_error = TRUE)

#### Arguments

- `keyring_if_error`:

  Logical. Launch pop-up for keyring if token is not valid or fails to
  call REDCap. Max 3 attempts. Default is `TRUE`.

------------------------------------------------------------------------

### `REDCapSyncProject$url_launch()`

opens links in browser.

#### Usage

    REDCapSyncProject$url_launch(link_type = "home", open_browser = TRUE)

#### Arguments

- `link_type`:

  Character. REDCap link type: "base", "home", "record_home",
  "records_dashboard", "api", "api_playground", "codebook",
  "user_rights", "setup", "logging", "designer", "dictionary",
  "data_quality", or "identifiers".

- `open_browser`:

  Logical. Open link in browser. Default is `TRUE`.

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

  Character. Record ID.

- `page`:

  Character. REDCap form/instrument name.

- `instance`:

  Character. Repeating instance number.

- `open_browser`:

  Logical. Open link in browser. Default is `TRUE`.

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

  Data frame in raw coded format ready to upload to REDCap.

## Examples

``` r
# Load a test project
projects$test_names() # available test projects
#>  [1] "TEST_CLASSIC"              "TEST_REPEATING"           
#>  [3] "TEST_LONGITUDINAL"         "TEST_MULTIARM"            
#>  [5] "TEST_EDGE"                 "TEST_DATA"                
#>  [7] "TEST_CANCER"               "TEST_REDCAPR_SIMPLE"      
#>  [9] "TEST_REDCAPR_LONGITUDINAL" "TEST_REDCAPR_CLIN_TRIAL"  
project <- setup_project("TEST_CLASSIC", dir_path = tempdir())
#> ! No cached projects... use `setup_project(...)`
#> ✔ Directory is Valid! /tmp/RtmpU5CYEQ
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API
#> Warning: Selecting ‘env’ backend. Secrets are stored in environment variables
#> ! No valid token in session: Sys.getenv('REDCAPSYNC_TEST_CLASSIC')

# Sync data from REDCap
project$sync()
#> ℹ TEST projects do not communicate with the API
#> ✔ Saved TEST_CLASSIC_text.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_text.xlsx
#> ✔ Saved TEST_CLASSIC_other.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_other.xlsx
#> ✔ Saved TEST_CLASSIC_cancer.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_cancer.xlsx
#> ✔ Saved TEST_CLASSIC_forms.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_forms.xlsx
#> ✔ Saved TEST_CLASSIC_fields.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_fields.xlsx
#> ✔ Saved TEST_CLASSIC_choices.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_choices.xlsx
#> ✔ Saved TEST_CLASSIC_missing_codes.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_missing_codes.xlsx
#> ✔ Saved TEST_CLASSIC_users.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_users.xlsx
#> ✔ Saved TEST_CLASSIC_dataset_details.xlsx: /tmp/RtmpU5CYEQ/REDCap/TEST_CLASSIC/TEST_CLASSIC_dataset_details.xlsx
#> ✔ Saved TEST_CLASSIC_REDCapSync.xlsx: /tmp/RtmpU5CYEQ/output/TEST_CLASSIC_REDCapSync.xlsx

# Access data and metadata
head(project$data$text)
#>   record_id  var_text_only    var_last_name var_birth_date var_text_ontology
#> 1         1 Just some text             Rose     1995-12-01     ctcae5:C57293
#> 2         2           <NA>           Gorthy     1971-11-21              <NA>
#> 3         3           <NA>           Parker     1968-10-04              <NA>
#> 4         4           <NA>           Thomas     1946-02-19              <NA>
#> 5         5           <NA> Oversole-Lutters     1951-04-01              <NA>
#> 6         6           <NA>           Fisher     1997-09-26              <NA>
#>   var_text_date_dmy var_text_date_mdy var_text_date_ymd
#> 1        2025-11-19        2025-11-19        2025-11-19
#> 2        2024-04-23        2024-06-22        2024-05-11
#> 3        2024-03-28        2024-04-14        2024-10-26
#> 4        2024-11-21        2024-05-23        2024-12-31
#> 5        2024-10-08        2024-04-17        2024-09-09
#> 6        2024-11-08        2024-01-31        2024-03-16
#>   var_text_datetime_dmy_hm var_text_datetime_mdy_hm var_text_datetime_ymd_hm
#> 1      2025-11-19 19:55:00      2025-11-19 19:55:00                  Unknown
#> 2      2024-08-30 15:30:00      2024-06-18 09:20:00         2024-10-20 14:45
#> 3      2024-08-28 18:49:00      2024-12-15 18:55:00         2024-08-06 11:24
#> 4      2024-10-06 16:49:00      2024-01-26 12:03:00         2024-03-24 01:23
#> 5      2024-06-07 09:43:00      2024-06-13 01:07:00         2024-12-05 14:51
#> 6      2024-11-15 17:58:00      2024-07-17 00:09:00         2024-04-01 08:17
#>   var_text_datetime_mdy_hms var_text_datetime_ymd_hms      var_text_email
#> 1       2025-11-19 19:55:20       2025-11-19 19:55:20   brandon@gmail.com
#> 2       2024-04-16 08:36:17       2024-07-11 07:58:51  olsen@fakemail.com
#> 3       2024-02-28 03:36:58       2024-11-16 14:36:39  knott@fakemail.com
#> 4       2024-06-06 14:42:10       2024-05-31 02:53:51 garcia@fakemail.com
#> 5       2024-12-25 13:38:05       2024-10-13 19:33:21   cain@fakemail.com
#> 6       2024-08-23 18:04:22       2024-05-09 09:25:04   zusi@fakemail.com
#>   var_text_integer var_text_letters var_text_number var_text_number_1dec
#> 1                5              ABC         123.565                  1.1
#> 2               82                c              34                   74
#> 3               32                u              18                 97.7
#> 4               49                j              61                 43.2
#> 5               29                m              12                 71.7
#> 6               53                d              51                   34
#>   var_text_number_2dec var_text_phone_nam var_text_time_hms var_text_time_hm
#> 1                 <NA>     (215) 900-6000          19:56:08         19:56:00
#> 2                 53.3               <NA>          11:14:28         05:11:00
#> 3                80.91               <NA>          14:42:43         00:50:00
#> 4                85.45               <NA>          04:30:38         05:31:00
#> 5                65.79               <NA>          19:55:00         01:58:00
#> 6                83.68               <NA>          07:28:19         14:34:00
#>   var_text_zipcode text_complete
#> 1            19116    Unverified
#> 2             <NA>      Complete
#> 3             <NA>      Complete
#> 4             <NA>    Unverified
#> 5             <NA>    Incomplete
#> 6             <NA>    Incomplete
project$metadata$fields[1:5, ]
#>          field_name form_name section_header field_type field_label
#> 1         record_id      text           <NA>       text   Record ID
#> 2     var_text_only      text           <NA>       text   Text Only
#> 3     var_last_name      text           <NA>       text   Last Name
#> 4    var_birth_date      text           <NA>       text  Birth Date
#> 5 var_text_ontology      text           <NA>       text    Ontology
#>   select_choices_or_calculations       field_note
#> 1                           <NA> Note about field
#> 2                           <NA> Note about field
#> 3                           <NA> Note about field
#> 4                           <NA> Note about field
#> 5                BIOPORTAL:CTCAE Note about field
#>   text_validation_type_or_show_slider_number text_validation_min
#> 1                                       <NA>                <NA>
#> 2                                       <NA>                <NA>
#> 3                                       <NA>                <NA>
#> 4                                   date_dmy                <NA>
#> 5                                       <NA>                <NA>
#>   text_validation_max identifier branching_logic required_field
#> 1                <NA>       <NA>            <NA>           <NA>
#> 2                <NA>       <NA>            <NA>           <NA>
#> 3                <NA>          y            <NA>           <NA>
#> 4                <NA>          y            <NA>           <NA>
#> 5                <NA>       <NA>            <NA>           <NA>
#>   custom_alignment question_number matrix_group_name matrix_ranking
#> 1             <NA>            <NA>              <NA>           <NA>
#> 2             <NA>            <NA>              <NA>           <NA>
#> 3             <NA>            <NA>              <NA>           <NA>
#> 4             <NA>            <NA>              <NA>           <NA>
#> 5             <NA>            <NA>              <NA>           <NA>
#>   field_annotation
#> 1             <NA>
#> 2             <NA>
#> 3             <NA>
#> 4             <NA>
#> 5             <NA>

forms <- project$metadata$forms # unchanged metadata
fields <- project$metadata$fields # unchanged metadata
choices <- project$metadata$choices # unchanged metadata

users <- project$redcap$users # unchanged users
log <- project$redcap$log # unchanged log

project$test_token()
#> ℹ TEST projects do not communicate with the API

dataset <- project$load_dataset("REDCapSync")
```
