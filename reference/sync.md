# Synchronize REDCap Data

Syncs with REDCap via `project` object that user defined with
[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

## Usage

``` r
sync(
  project_names = NULL,
  summarize = TRUE,
  hard_check = FALSE,
  hard_reset = FALSE
)
```

## Arguments

- project_names:

  character vector of project project_names previously setup. If NULL,
  will get all from
  [`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)

- summarize:

  Logical (TRUE/FALSE). If TRUE, summarizes data to directory.

- hard_check:

  Will check REDCap even if not due (see `sync_frequency` parameter from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md))

- hard_reset:

  Logical that forces a fresh update if TRUE. Default is `FALSE`.

## Value

Invisibly returns last project object (R6
[REDCapSyncProject](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSyncProject.md)
class).

## Details

Syncs all projects by default but can be used to hands-free sync one or
defined set projects. This is not intended to return project object.
User should use `load_project("project_name")`. However, by default will
invisibly return the last project in the set of project_names.

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# if you setup 3 projects "ONE", "TWO", "THREE" using [setup_project()]...
sync() # will check all three projects for updates if due for sync
sync("TWO") # will only check and sync project TWO
project <- sync("TWO") # can also be used for invisible return ...
sync(c("ONE","THREE")) # will only check and sync projects ONE and THREE
project <- sync(c("ONE","THREE")) # invisible return of THREE ...
} # }
```
