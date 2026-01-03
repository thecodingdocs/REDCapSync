# REDCapSync Encapsulated Project Object

[R6](https://r6.r-lib.org/reference/R6Class.html) project object for
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)
Main class for managing REDCap data, metadata, and sync operations.
Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
For exisiting projects, use
[`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
For offline examples use
[`load_test_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

## Value

An R6ClassGenerator

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.'

## Active bindings

- `project_name`:

  Read-only character string of project_name as assigned from
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

- `data`:

  Read-only named list where each name is an instrument name. See
  REDCapSync_project\$sync() or see
  [`REDCapSync_project$sync()`](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync_project.html#method-REDCapSync_project-sync)

- `.internal_project`:

  (`character(1)`)  
  Entire internal project object for more advanced/custom workflows

## Methods

### Public methods

- [`REDCapSync_project$new()`](#method-REDCapSync_project-new)

- [`REDCapSync_project$info()`](#method-REDCapSync_project-info)

- [`REDCapSync_project$sync()`](#method-REDCapSync_project-sync)

- [`REDCapSync_project$add_summary()`](#method-REDCapSync_project-add_summary)

- [`REDCapSync_project$remove_summaries()`](#method-REDCapSync_project-remove_summaries)

- [`REDCapSync_project$generate_summary()`](#method-REDCapSync_project-generate_summary)

- [`REDCapSync_project$add_field()`](#method-REDCapSync_project-add_field)

- [`REDCapSync_project$remove_fields()`](#method-REDCapSync_project-remove_fields)

- [`REDCapSync_project$summarize()`](#method-REDCapSync_project-summarize)

- [`REDCapSync_project$save_summary()`](#method-REDCapSync_project-save_summary)

- [`REDCapSync_project$save()`](#method-REDCapSync_project-save)

- [`REDCapSync_project$show_metadata()`](#method-REDCapSync_project-show_metadata)

- [`REDCapSync_project$show_data()`](#method-REDCapSync_project-show_data)

- [`REDCapSync_project$show_token()`](#method-REDCapSync_project-show_token)

- [`REDCapSync_project$test_token()`](#method-REDCapSync_project-test_token)

- [`REDCapSync_project$url_launch()`](#method-REDCapSync_project-url_launch)

- [`REDCapSync_project$upload()`](#method-REDCapSync_project-upload)

- [`REDCapSync_project$.internal()`](#method-REDCapSync_project-.internal)

------------------------------------------------------------------------

### Method `new()`

Active binding are read-only

The end user will not see `project$new()`. This is handled internally.
Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
The remain methods will be accessible to any user.

#### Usage

    REDCapSync_project$new(project)

#### Arguments

- `project`:

  a list object meant to be stored internally within R6

------------------------------------------------------------------------

### Method `info()`

Print project metadata

#### Usage

    REDCapSync_project$info()

------------------------------------------------------------------------

### Method [`sync()`](https://thecodingdocs.github.io/REDCapSync/reference/sync.md)

Updates the REDCap data for (`project` object) by checking REDCap log.

#### Usage

    REDCapSync_project$sync(
      summarize = TRUE,
      save_to_dir = TRUE,
      hard_check = FALSE,
      hard_reset = FALSE
    )

#### Arguments

- `summarize`:

  Logical (TRUE/FALSE). If TRUE, summarizes data to directory.

- `save_to_dir`:

  Logical (TRUE/FALSE). If TRUE, saves the updated data to the
  directory. Default is `TRUE`.

- `hard_check`:

  Will check REDCap even if not due (see `sync_frequency` parameter from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md))

- `hard_reset`:

  Logical. If `TRUE`, overwrite existing summary files with the same
  name. Default is `FALSE`.

#### Returns

Messages for confirmation.

------------------------------------------------------------------------

### Method `add_summary()`

Add a new summary entry

#### Usage

    REDCapSync_project$add_summary(
      summary_name,
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
      with_links = TRUE,
      separate = FALSE,
      use_csv = FALSE,
      dir_other = NULL,
      file_name = NULL,
      hard_reset = FALSE
    )

#### Arguments

- `summary_name`:

  Character. The name of the summary from which to generate the summary.
  \*If you provide `summary_name` all other parameters are inherited
  according to what was set with `add_project_summary`.

- `transformation_type`:

  Character vector. Default is "default". Also have "none", "flat",
  "merge_non_repeating" "default" first merges non-repeating and if
  there are repeating forms it merges non-repeating variables to the
  right of repeating instruments "flat" is one-record, one-row, even if
  there are repeating forms "none" does not transform anything
  "merge_non_repeating" still merges all non-repeating instruments but
  does not merge them to repeating instruments

- `merge_form_name`:

  A character string representing the name of the merged form. Default
  is "merged".

- `filter_field`:

  Character. The name of the field in the database to filter on.

- `filter_choices`:

  Vector. The values of `filter_field` used to define the summary.

- `filter_list`:

  Vector. The values of `filter_field` used to define the summary.

- `filter_strict`:

  Logical. If `TRUE`, all forms will be filtered by criteria. If
  `FALSE`, will convert original filter to id column and filter all
  other forms by that record. Default is `TRUE`.

- `field_names`:

  Character vector. Names of specific fields to include in the summary.
  Default is `NULL`, which includes all fields.

- `form_names`:

  Character vector. Names of forms to include in the summary. Default is
  `NULL`, which includes all forms.

- `exclude_identifiers`:

  Logical. Whether to exlude identifiers in the data in the summary.
  Default is `TRUE`.

- `exclude_free_text`:

  Logical for excluding free text. Default is `FALSE`.

- `date_handling`:

  character string. One of `none`,`exclude_dates`,
  `random_shift_by_record`, `random_shift_by_project`, `zero_by_record`,
  or `zero_by_project` random shift is +/- 90 unless changed with
  options

- `labelled`:

  Logical. If `TRUE`, the data will be converted to labelled. Default is
  `TRUE`.

- `clean`:

  Logical. If `TRUE`, the data will be cleaned before summarizing.
  Default is `TRUE`. If missing codes present AND number or date type, R
  will convert to those to NA and would make that variable not upload
  compatible

- `drop_blanks`:

  Logical. If `TRUE`, records with blank fields will be dropped. Default
  is `TRUE`.

- `drop_missing_codes`:

  Logical. If `TRUE`, will convert missing codes to NA. Default is
  `FALSE`.

- `drop_others`:

  Character vector of other values that should be dropped.

- `include_metadata`:

  Logical. If `TRUE`, metadata will be included in the summary. Default
  is `TRUE`.

- `include_records`:

  Logical. If `TRUE`, a record summary will be included in the generated
  summary. Default is `TRUE`.

- `include_users`:

  Logical. If `TRUE`, user-related information will be included in the
  summary. Default is `TRUE`.

- `include_log`:

  Logical. If `TRUE`, the log of changes will be included in the
  summary. Default is `TRUE`.

- `annotate_from_log`:

  Logical. If `TRUE`, the metadata, users, and records will be annotated
  using the log. Default is `TRUE`.

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

  Character. The directory where the summary file will be saved. Default
  is the `output` folder within the database directory.

- `file_name`:

  Character. The base name of the file where the summary will be saved.
  Default is `<project_name>_<summary_name>`.

- `hard_reset`:

  Logical. If `TRUE`, overwrite existing summary files with the same
  name. Default is `FALSE`.

------------------------------------------------------------------------

### Method `remove_summaries()`

Clear all project summaries

#### Usage

    REDCapSync_project$remove_summaries(summary_names = NULL)

#### Arguments

- `summary_names`:

  One or more summary names. Default is `NULL`.

------------------------------------------------------------------------

### Method `generate_summary()`

Add a new summary entry

#### Usage

    REDCapSync_project$generate_summary(summary_name = "REDCapSync", envir = NULL)

#### Arguments

- `summary_name`:

  Character. The name of the summary from which to generate the summary.
  \*If you provide `summary_name` all other parameters are inherited
  according to what was set with `add_project_summary`.

- `envir`:

  environment variable

------------------------------------------------------------------------

### Method `add_field()`

Add a new summary entry

#### Usage

    REDCapSync_project$add_field(
      field_name,
      form_name,
      field_type,
      field_type_R = NA,
      field_label = NA,
      select_choices_or_calculations = NA,
      field_note = NA,
      identifier = "",
      units = NA,
      data_func = NA
    )

#### Arguments

- `field_name`:

  Character. The name of the field to which the transformation will be
  applied.

- `form_name`:

  Character. The name of the form containing the field.

- `field_type`:

  Character. The type of the field in REDCap (e.g., "text", "checkbox",
  "dropdown").

- `field_type_R`:

  Character. The corresponding R data type for the field. Default is
  `NA`.

- `field_label`:

  Character. The label for the field. Default is `NA`.

- `select_choices_or_calculations`:

  Character. A string specifying the choices (for dropdown, radio, or
  checkbox fields) or calculations (for calculated fields). Default is
  `NA`.

- `field_note`:

  Character. An optional note or comment for the field. Default is `NA`.

- `identifier`:

  Character. A string indicating whether the field is an identifier
  (e.g., "Y" for yes). Default is an empty string (`""`).

- `units`:

  Character. The units of measurement for the field, if applicable.
  Default is `NA`.

- `data_func`:

  Function or NA. An optional function to transform or validate the data
  in the field. Default is `NA`.

------------------------------------------------------------------------

### Method `remove_fields()`

Removes summary entry

#### Usage

    REDCapSync_project$remove_fields(field_names = NULL)

#### Arguments

- `field_names`:

  Character vector. Names of specific fields to include in the summary.
  Default is `NULL`, which includes all fields.

------------------------------------------------------------------------

### Method `summarize()`

summarize project and save to Excel

#### Usage

    REDCapSync_project$summarize(hard_reset = FALSE)

#### Arguments

- `hard_reset`:

  Logical. If `TRUE`, overwrite existing summary files with the same
  name. Default is `FALSE`.

------------------------------------------------------------------------

### Method `save_summary()`

save summary to Excel

#### Usage

    REDCapSync_project$save_summary(summary_name)

#### Arguments

- `summary_name`:

  Character. The name of the summary from which to generate the summary.
  \*If you provide `summary_name` all other parameters are inherited
  according to what was set with `add_project_summary`.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Add a new summary entry

#### Usage

    REDCapSync_project$save()

------------------------------------------------------------------------

### Method `show_metadata()`

Returns list of data or the specified form.

#### Usage

    REDCapSync_project$show_metadata(type = NULL, envir = NULL)

#### Arguments

- `type`:

  string of either "fields","forms", or "choices"

- `envir`:

  environment variable

------------------------------------------------------------------------

### Method `show_data()`

Returns list of data or the specified form.

#### Usage

    REDCapSync_project$show_data(form = NULL, envir = NULL)

#### Arguments

- `form`:

  string of raw form name such as "survey_one"

- `envir`:

  environment variable

------------------------------------------------------------------------

### Method `show_token()`

Displays the REDCap API token currently stored in the session as an
environment variable. It's essentially a wrapper for
Sys.getenv("YOUR_TOKEN_NAME"), but it also validates that the token is
formatted like a REDCap token and provides messgaes if not valid. The
token is not returned as an R object to maintain security.

#### Usage

    REDCapSync_project$show_token()

------------------------------------------------------------------------

### Method `test_token()`

test connection via communication with API

#### Usage

    REDCapSync_project$test_token()

------------------------------------------------------------------------

### Method `url_launch()`

opens links in browser

#### Usage

    REDCapSync_project$url_launch(link_type = "home", open_browser = TRUE)

#### Arguments

- `link_type`:

  choose one of "base", "home", "record_home", "records_dashboard",
  "api", "api_playground", "codebook", "user_rights", "setup",
  "logging", "designer", "dictionary", "data_quality", "identifiers"

- `open_browser`:

  logical for launching the link in internet browser

------------------------------------------------------------------------

### Method `upload()`

This will only overwrite and new data. It will not directly delete any
data. Because this is a function that can mess up your data, use it very
carefilly. Remember all changes are saved in the REDCap log if there's
an issue. Missing rows and columns are allowed!

#### Usage

    REDCapSync_project$upload(to_be_uploaded, batch_size = 500L)

#### Arguments

- `to_be_uploaded`:

  data.frame in raw coded form.

- `batch_size`:

  numeric of how big the REDCap batch upload is. Default 500.

------------------------------------------------------------------------

### Method `.internal()`

returns internal list

#### Usage

    REDCapSync_project$.internal()
