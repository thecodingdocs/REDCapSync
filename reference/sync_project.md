# Synchronize REDCap Data

Updates the REDCap database (`project` object) by fetching the latest
data from the REDCap server.

## Usage

``` r
sync_project(
  project,
  summarize = TRUE,
  save_to_dir = TRUE,
  hard_check = FALSE,
  hard_reset = FALSE,
  silent = FALSE
)
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

- summarize:

  Logical (TRUE/FALSE). If TRUE, summarizes data to directory.

- save_to_dir:

  Logical (TRUE/FALSE). If TRUE, saves the updated data to the
  directory. Default is `TRUE`.

- hard_check:

  Will check REDCap even if not due (see `sync_frequency` parameter from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md))

- hard_reset:

  Logical that forces a fresh update if TRUE. Default is `FALSE`.

- silent:

  Logical (TRUE/FALSE). For messages.

## Value

Messages for confirmation.

## Details

This function updates the REDCap database by fetching the latest data
from the REDCap server. It supports various options such as forcing a
fresh update, checking logs for a specified number of days, and
retrieving files from REDCap. The function can also handle metadata-only
updates and batch processing.

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.

Other db_functions:
[`sync_all()`](https://thecodingdocs.github.io/REDCapSync/reference/sync_all.md)
