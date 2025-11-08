# Clean to Raw REDCap forms

Clean to Raw REDCap forms

## Usage

``` r
labelled_to_raw_form(form, project)
```

## Arguments

- form:

  data.frame of labelled REDCap to be converted to raw REDCap (for
  uploads)

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

## Value

project object that has been filtered to only include the specified
records
