# Synchronize REDCap Data

Updates the REDCap database (`project` object) by fetching the latest
data from the REDCap server.

## Usage

``` r
sync_all(
  short_names = NULL,
  summarize = TRUE,
  hard_check = FALSE,
  hard_reset = FALSE,
  silent = FALSE
)
```

## Arguments

- short_names:

  character vector of project short_names previously setup. If = NULL,
  will get all from
  [`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)

- summarize:

  Logical (TRUE/FALSE). If TRUE, summarizes data to directory.

- hard_check:

  Will check REDCap even if not due (see `sync_frequency` parameter from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md))

- hard_reset:

  Logical that forces a fresh update if TRUE. Default is `FALSE`.

- silent:

  Logical (TRUE/FALSE). For messages.

## Value

projects synced to dir.

## Details

syncs multiple projects as defined

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.

Other db_functions:
[`sync_project()`](https://thecodingdocs.github.io/REDCapSync/reference/sync_project.md)
