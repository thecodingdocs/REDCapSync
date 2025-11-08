# Add a Subset to a REDCap Database

**\[experimental\]** Creates a summary of the main REDCap database
(`project`) based on specific filter criteria and saves it to a
specified directory. The summary can be further customized with
additional forms, fields, and deidentification options.

## Usage

``` r
add_project_summary(
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
  include_records = TRUE,
  include_users = TRUE,
  include_log = FALSE,
  annotate_from_log = TRUE,
  with_links = TRUE,
  separate = FALSE,
  use_csv,
  dir_other = file.path(project$dir_path, "output"),
  file_name = paste0(project$short_name, "_", summary_name),
  hard_reset = FALSE
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

  Character. The name of the summary to create.

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

  character string. One of `none`,`lowest-overall-zero`,
  `lowest-record-zero`, `shuffle-record-randomly`, or zero date date in
  form of `2012-12-05`

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

- include_records:

  Logical. If `TRUE`, a record summary will be included in the generated
  summary. Default is `TRUE`.

- include_users:

  Logical. If `TRUE`, user-related information will be included in the
  summary. Default is `TRUE`.

- include_log:

  Logical. If `TRUE`, the log of changes will be included in the
  summary. Default is `TRUE`.

- annotate_from_log:

  Logical. If `TRUE`, the metadata, users, and records will be annotated
  using the log. Default is `TRUE`.

- with_links:

  Optional logical (TRUE/FALSE) for including links in Excel sheets.
  Default is `FALSE`.

- separate:

  Optional logical (TRUE/FALSE) separating each form into separate files
  as opposed to multi-tab Excel. Default is `FALSE`.

- use_csv:

  Logical (TRUE/FALSE). If TRUE, uses CSV files for data storage.
  Default is `FALSE`.

- dir_other:

  Character. The directory where the summary file will be saved. Default
  is the `output` folder within the database directory.

- file_name:

  Character. The base name of the file where the summary will be saved.
  Default is `<project$short_name>_<summary_name>`.

- hard_reset:

  Logical. If `TRUE`, overwrite existing summary files with the same
  name. Default is `FALSE`.

## Value

A modified `project` object that includes the newly created summary. The
summary is also saved as a file in the specified directory.

## Details

This function filters the main REDCap database using the specified
`filter_field` and `filter_choices`, then creates a new summary with
optional deidentification. It can be customized to include only specific
forms or fields. The resulting summary is saved to a file for future
use.

## See also

[`save_project`](https://thecodingdocs.github.io/REDCapSync/reference/save-deleteproject.md)
for saving the main database or summaries.
