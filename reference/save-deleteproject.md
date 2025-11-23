# Save or Delete project file from the directory

This will save/delete the "\<short_name\>\_REDCapSync.RData" file in the
given project directories R_objects folder. These are optional functions
given that `save_project` is a also handled by a default parameter in
`sync_project.`

## Usage

``` r
save_project(project, silent = FALSE)
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

## Value

Message

## Details

delete_project will not delete any other files from that directory. The
user must delete any other files manually.

## See also

Other project object:
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
