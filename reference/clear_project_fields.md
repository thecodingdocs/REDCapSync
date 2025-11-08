# clear_project_fields

**\[experimental\]** Clears and project fields that were added with
`add_project_fields()` or if
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
contained `add_default_fields = TRUE`

## Usage

``` r
clear_project_fields(project)
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
