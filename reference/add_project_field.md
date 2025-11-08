# Add Field Transformation to the Database

**\[experimental\]** Adds a new field transformation to the REDCap
database (`project`). This allows users to define custom transformations
for a specific field in a form, including its type, label, choices, and
associated function for data manipulation.

## Usage

``` r
add_project_field(
  project,
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
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

- field_name:

  Character. The name of the field to which the transformation will be
  applied.

- form_name:

  Character. The name of the form containing the field.

- field_type:

  Character. The type of the field in REDCap (e.g., "text", "checkbox",
  "dropdown").

- field_type_R:

  Character. The corresponding R data type for the field. Default is
  `NA`.

- field_label:

  Character. The label for the field. Default is `NA`.

- select_choices_or_calculations:

  Character. A string specifying the choices (for dropdown, radio, or
  checkbox fields) or calculations (for calculated fields). Default is
  `NA`.

- field_note:

  Character. An optional note or comment for the field. Default is `NA`.

- identifier:

  Character. A string indicating whether the field is an identifier
  (e.g., "Y" for yes). Default is an empty string (`""`).

- units:

  Character. The units of measurement for the field, if applicable.
  Default is `NA`.

- data_func:

  Function or NA. An optional function to transform or validate the data
  in the field. Default is `NA`.

## Value

The updated `project` object with the field transformation added.

## Details

This function facilitates the addition of a new field transformation to
a REDCap database. The transformation includes metadata such as the
field's type, label, and choices, along with an optional function to
process the data. This is particularly useful for customizing or
extending the functionality of existing REDCap forms and fields.

## See also

[`save_project`](https://thecodingdocs.github.io/REDCapSync/reference/save-deleteproject.md)
for saving the database or summaries.
