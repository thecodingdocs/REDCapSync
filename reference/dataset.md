# REDCapSync Dataset Object

**\[experimental\]** [R6](https://r6.r-lib.org/reference/R6Class.html)
object representing a standardized dataset generated from a
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)
project. Use this object to inspect transformed REDCap data, export
datasets, and assign prepared analysis tables into the calling
environment.

## Value

An R6 `REDCapSyncDataset` object containing dataset output, metadata,
records, user information, and optional REDCap log data.

## Details

A `REDCapSyncDataset` can be created ad-hoc from a project with
`dataset <- project$generate_dataset()`. For reusability, you can also
define with `project$add_dataset()` and then load with
`dataset <- project$load_dataset()`.

You can add variables manually if you want them to be passed to Excel.
But `add_field` is a feature in development.

### Typical workflow

    setup project from test object
    project <- setup_project(
      project_name = "TEST_CLASSIC",
      dir_path = tempdir()
    )
    # Create and save a filtered dataset
    project$add_dataset(
      dataset_name = "analysis_set",
      filter_field = "var_yesno",
      filter_choices = "Yes",
      field_names = c("ecog_at_diagnosis", "stage_at_diagnosis")
    )
    # generate dataset for R environment
    dataset <- project$load_dataset("analysis_set")
    # optional send to global environment
    dataset$to_envir(globalenv()) # keep in mind potential name conflicts
    # Optional modify (final save depends on what is in the dataset object)
    dataset$data$merged$stage_2 <- dataset$data$merged$stage_at_diagnosis == "II"
    # save to directory
    dataset$save() # can specify `dir_other`, by default saves to output folder

This object is designed for users who want a stable dataset output from
REDCap without modifying the underlying project. This is also used
behind-the-scenes in the RosyREDCap shiny app.

### Key features

- Stores a project-specific dataset definition and resulting data

- Keeps metadata, record details, user information, and comments in sync

- Saves datasets in Excel or CSV formats with optional hyperlinks

- Exports dataset components to a user-specified environment

## See also

[project](https://thecodingdocs.github.io/REDCapSync/reference/project.md)
for using the project objects
[`vignette("Datasets", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Datasets.md)
[`vignette("RosyREDCap", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/RosyREDCap.md)

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

- [`REDCapSyncDataset$new()`](#method-REDCapSyncDataset-initialize)

- [`REDCapSyncDataset$print()`](#method-REDCapSyncDataset-print)

- [`REDCapSyncDataset$preview()`](#method-REDCapSyncDataset-preview)

- [`REDCapSyncDataset$save()`](#method-REDCapSyncDataset-save)

- [`REDCapSyncDataset$to_envir()`](#method-REDCapSyncDataset-to_envir)

------------------------------------------------------------------------

### `REDCapSyncDataset$new()`

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
      exclude_identifiers = FALSE,
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

  Project object from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

- `dataset_name`:

  Character. Name of the dataset to generate or load. If the dataset
  already exists in the project, the existing definition is reused.

- `transformation_type`:

  Character. Data transformation strategy: "default" (preferred merged
  output), "none" (raw data structure), or "merge_non_repeating" (merge
  only non-repeating forms). Default is "default".

- `merge_form_name`:

  Character. Name used for merged non-repeating records. Default is
  "merged".

- `filter_field`:

  Character. Field used for filtering the dataset.

- `filter_choices`:

  Vector. Allowed values for `filter_field`.

- `filter_list`:

  List. Named list mapping field names to allowed values. Use instead of
  `filter_field`/`filter_choices` for more complex filters.

- `filter_strict`:

  Logical. If `TRUE`, filters are applied to every form. If `FALSE`,
  filters apply only to the record identifier. Default is `TRUE`.

- `field_names`:

  Character vector. Variables to include in the dataset. Default is
  `NULL` (all fields).

- `form_names`:

  Character vector. Forms to include in the dataset. Default is `NULL`
  (all forms).

- `exclude_identifiers`:

  Logical. Remove identifier fields. Default is `TRUE`.

- `exclude_free_text`:

  Logical. Remove free text fields. Default is `FALSE`.

- `date_handling`:

  Character. Date handling method: "none", "exclude_dates",
  "random_shift_by_record", "random_shift_by_project", "zero_by_record",
  or "zero_by_project". Default is "none".

- `labelled`:

  Logical. Convert values to labelled vectors if `TRUE`. Default is
  `TRUE`.

- `clean`:

  Logical. Clean the dataset by standardizing missing values and blanks.
  Default is `TRUE`.

- `drop_blanks`:

  Logical. Drop records with blank fields. Default is `FALSE`.

- `drop_missing_codes`:

  Logical. Convert REDCap missing codes to `NA`. Default is `FALSE`.

- `drop_others`:

  Character vector of additional values to remove.

- `include_metadata`:

  Logical. Include field metadata in the dataset. Default is `TRUE`.

- `include_users`:

  Logical. Include user information in the dataset. Default is `TRUE`.

- `include_records`:

  Logical. Include record-level details. Default is `TRUE`.

- `include_log`:

  Logical. Include REDCap activity log details. Default is `FALSE`.

- `annotate_from_log`:

  Logical. Annotate metadata and records using the change log. Default
  is `TRUE`.

- `include_comments`:

  Logical. Include REDCap comments. Default is `FALSE`.

------------------------------------------------------------------------

### `REDCapSyncDataset$print()`

Print some key dataset information

#### Usage

    REDCapSyncDataset$print()

------------------------------------------------------------------------

### `REDCapSyncDataset$preview()`

Saves temporary excel and opens as a preview

#### Usage

    REDCapSyncDataset$preview(with_links = TRUE)

#### Arguments

- `with_links`:

  Logical. Include hyperlinks in Excel exports. Default is `TRUE`.

------------------------------------------------------------------------

### `REDCapSyncDataset$save()`

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

  Logical. Include hyperlinks in Excel exports. Default is `TRUE`.

- `separate`:

  Logical. Save each form as a separate file instead of a multi-sheet
  workbook. Default is `FALSE`.

- `use_csv`:

  Logical. Write CSV files instead of Excel. Default is `FALSE`.

- `dir_other`:

  Character. Directory where the dataset file should be saved. Defaults
  to the project's output folder.

- `file_name`:

  Character. Base file name for saved datasets. Defaults to
  `<project_name>_<dataset_name>`.

------------------------------------------------------------------------

### `REDCapSyncDataset$to_envir()`

export dataset to envir of your choosing. Keep in mind potential name
conflicts

#### Usage

    REDCapSyncDataset$to_envir(envir = NULL)

#### Arguments

- `envir`:

  Environment to assign exported dataset objects. Default is `NULL`.

## Examples

``` r
project <- load_project("TEST_CLASSIC")
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API

dataset <- project$generate_dataset(
  dataset_name = "stage_2_patients",
  filter_field = "stage_at_diagnosis",
  filter_choices = "II",
  field_names = c("ecog_at_diagnosis", "stage_at_diagnosis")
 )

dataset$save(dir_other = tempdir())
#> ✔ Saved TEST_CLASSIC_stage_2_patients.xlsx: /tmp/RtmpFcqxfP/TEST_CLASSIC_stage_2_patients.xlsx
```
