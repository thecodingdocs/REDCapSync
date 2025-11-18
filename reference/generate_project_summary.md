# Generate a Summary from a Subset Name

**\[experimental\]** Generates a summary from a predefined summary of
data within a REDCap project. The summary can be customized based on
various options, such as cleaning the data, including metadata, and
annotating metadata.

## Usage

``` r
generate_project_summary(
  project,
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
  upload_compatible = TRUE,
  labelled = TRUE,
  clean = TRUE,
  drop_blanks = FALSE,
  drop_missings = FALSE,
  drop_others = NULL,
  include_metadata = TRUE,
  include_users = TRUE,
  include_records = TRUE,
  include_log = FALSE,
  annotate_from_log = TRUE,
  internal_use = FALSE
)
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

- summary_name:

  Character. The name of the summary from which to generate the summary.
  \*If you provide `summary_name` all other parameters are inherited
  according to what was set with `add_project_summary`.

- transformation_type:

  Character vector. Default is "default". Also have "none", "flat",
  "merge-non-repeating" "default" first merges non-repeating and if
  there are repeating forms it merges non-repeating variables to the
  right of repeating instruments "flat" is one-record, one-row, even if
  there are repeating forms "none" does not transform anything
  "merge-non-repeating" still merges all non-repeating instruments but
  does not merge them to repeating instruments

- merge_form_name:

  A character string representing the name of the merged form. Default
  is "merged".

- filter_field:

  Character. The name of the field in the database to filter on.

- filter_choices:

  Vector. The values of `filter_field` used to define the summary.

- filter_list:

  Vector. The values of `filter_field` used to define the summary.

- filter_strict:

  Logical. If `TRUE`, all forms will be filtered by criteria. If
  `FALSE`, will convert original filter to id column and filter all
  other forms by that record. Default is `TRUE`.

- field_names:

  Character vector. Names of specific fields to include in the summary.
  Default is `NULL`, which includes all fields.

- form_names:

  Character vector. Names of forms to include in the summary. Default is
  `NULL`, which includes all forms.

- exclude_identifiers:

  Logical. Whether to exlude identifiers in the data in the summary.
  Default is `TRUE`.

- exclude_free_text:

  Logical for excluding free text. Default is `FALSE`.

- date_handling:

  character string. One of `none`,`exclude_dates`,
  `random_shift_by_record`, `random_shift_by_project`, `zero_by_record`,
  or `zero_by_project` random shift is +/- 90 unless changed with
  options

- upload_compatible:

  Logical. If `TRUE`, the data will be compatible with REDCap API
  upload. The main conflict is numeric or date variables in a project
  with missing codes while `clean` = `TRUE`. R converts these to `NA`.
  Default is `TRUE`.

- labelled:

  Logical. If `TRUE`, the data will be converted to labelled. Default is
  `TRUE`.

- clean:

  Logical. If `TRUE`, the data will be cleaned before summarizing.
  Default is `TRUE`.

- drop_blanks:

  Logical. If `TRUE`, records with blank fields will be dropped. Default
  is `TRUE`.

- drop_missings:

  Logical. If `TRUE`, will convert missing codes to NA. Default is
  `FALSE`.

- drop_others:

  Character vector of other values that should be dropped.

- include_metadata:

  Logical. If `TRUE`, metadata will be included in the summary. Default
  is `TRUE`.

- include_users:

  Logical. If `TRUE`, user-related information will be included in the
  summary. Default is `TRUE`.

- include_records:

  Logical. If `TRUE`, a record summary will be included in the generated
  summary. Default is `TRUE`.

- include_log:

  Logical. If `TRUE`, the log of changes will be included in the
  summary. Default is `TRUE`.

- annotate_from_log:

  Logical. If `TRUE`, the metadata, users, and records will be annotated
  using the log. Default is `TRUE`.

- internal_use:

  A logical flag (`TRUE` or `FALSE`). If `TRUE`, then will return
  data_list meant for internal use. Defaults to `FALSE`.

## Value

A list containing the generated summary based on the specified options.
The list includes filtered and cleaned data, metadata, and other summary
details.

## Details

This function allows you to generate a summary of data from a specific
summary of records within the REDCap project. The function provides
flexible options for cleaning, annotating, and including metadata, as
well as controlling whether to include record summaries, user
information, and logs.
