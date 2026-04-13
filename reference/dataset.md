# Standardized Dataset from REDCap Project

**\[experimental\]** [R6](https://r6.r-lib.org/reference/R6Class.html)
project object for
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)

## Value

An R6ClassGenerator which is used internally to create or load a dataset
object for the user

## See also

[`vignette("Datasets", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Datasets.md)
[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.'

## Public fields

- `data`:

  list of data where names are forms

- `metadata`:

  list of metadata

- `records`:

  data.frame of records with timestamps

- `users`:

  data.frame of users with timestamps

- `log`:

  data.frame of log

- `comments`:

  data.frame of comments

## Active bindings

- `project_details`:

  Read-only list of dataset details from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

- `dataset_details`:

  Read-only list of dataset details

- `redcap`:

  Read-only list of dataset details from
  [project](https://thecodingdocs.github.io/REDCapSync/reference/project.md)

- `links`:

  Read-only list of dataset details from
  [project](https://thecodingdocs.github.io/REDCapSync/reference/project.md)

## Methods

### Public methods

- [`REDCapSyncDataset$new()`](#method-REDCapSyncDataset-new)

- [`REDCapSyncDataset$print()`](#method-REDCapSyncDataset-print)

- [`REDCapSyncDataset$save()`](#method-REDCapSyncDataset-save)

- [`REDCapSyncDataset$to_envir()`](#method-REDCapSyncDataset-to_envir)

------------------------------------------------------------------------

### Method `new()`

The end user will not see `dataset$new()`. This is handled internally.
Users should construct objects using
[REDCapSyncProject](https://thecodingdocs.github.io/REDCapSync/reference/project.md).

#### Usage

    REDCapSyncDataset$new(
      project,
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
      include_users = TRUE,
      include_records = TRUE,
      include_log = FALSE,
      annotate_from_log = TRUE,
      include_comments = FALSE
    )

#### Arguments

- `project`:

  object from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

- `dataset_name`:

  Character. The name of the configured dataset from which to generate
  the dataset. \*If you provide `dataset_name` all other parameters are
  inherited according to what was set with `add_dataset`.

- `transformation_type`:

  Character scalar. How to transform data for the dataset. Default is
  "default". Other options are "none", "flat", "merge_non_repeating".
  "default" first merges non-repeating and if there are repeating forms,
  it merges non-repeating variables to the right of repeating
  instruments. "flat" is one-record, one-row, even if there are
  repeating forms. "none" does not transform anything.
  "merge_non_repeating" still merges all non-repeating instruments but
  does not merge them to repeating instruments.

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
  de-identification workflows. Default is `FALSE`.

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

- `include_users`:

  Logical. If `TRUE`, user-related information will be included in the
  dataset. Default is `TRUE`.

- `include_records`:

  Logical. If `TRUE`, a record dataset will be included in the generated
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

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print some key dataset information

#### Usage

    REDCapSyncDataset$print()

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Return flat list

#### Usage

    REDCapSyncDataset$save(
      with_links = TRUE,
      separate = FALSE,
      use_csv = FALSE,
      dir_other = NULL,
      file_name = NULL
    )

#### Arguments

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

------------------------------------------------------------------------

### Method `to_envir()`

export dataset to envir of your choosing. Keep in mind potential name
conflicts

#### Usage

    REDCapSyncDataset$to_envir(envir = NULL)

#### Arguments

- `envir`:

  environment variable such as
  [`globalenv()`](https://rdrr.io/r/base/environment.html)

## Examples

``` r
save_dir <- tempdir()
dataset <- load_project("TEST_CLASSIC")$generate_dataset("REDCapSync")
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API
#> ! REDCapSync is already a defined dataset
#> ℹ It will be loaded... other paramers ignored
# add quick custom variable
dataset$data$merged$letter_b <- dataset$data$merged$var_text_letters == "b"
# save data in custom location
dataset$save(dir_other = save_dir)
#> ℹ Saved 'TEST_CLASSIC_REDCapSync.xlsx'!
#>   /tmp/RtmpBwmbZi/TEST_CLASSIC_REDCapSync.xlsx
```
