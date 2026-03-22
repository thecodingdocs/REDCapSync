# Manage REDCapSync projects

`projects` is the central interface for managing REDCap projects in
**REDCapSync**. It provides a convenient, persistent registry of
projects that can be accessed across R sessions.

Each time a project is set up or synced, basic metadata is stored
locally so that projects can be reloaded and updated without
reconfiguration.

## Usage

``` r
projects
```

## Format

An object of class `REDCapSyncProjects` (inherits from `R6`) of length
11.

## Value

R6 object that can used be to access project objects

## Details

`projects` is implemented as a singleton R6 object and serves as the
main entry point to the REDCapSync workflow. All project-level
operations—such as setup, loading, syncing, and removal—are accessed
through this object.

A key advantage of this design is support for **method chaining**.
Because project methods return project objects, you can write concise,
readable workflows that operate in sequence:

    projects$load("my_project")$sync()$save_datasets()

The default location of the cache location is defined by using
R_USER_CACHE_DIR if set. Otherwise, it follows platform conventions via
[hoardr::hoardr](https://docs.ropensci.org/hoardr/reference/hoardr-package.html),
saving a file "R/REDCapSync/projects.rds". No direct project data is
stored in the cache. Notably, tokens and data are not stored here. The
key variables stored in the cache are...

- `project_name` - unique identifier for REDCapSync package

- `redcap_uri` - server location

- `token_name` - where to find token environment with
  [`Sys.getenv()`](https://rdrr.io/r/base/Sys.getenv.html)

- `dir_path` - where to saved project and associated files locally

- `project_id` - obtained from API call and "locks-in" the connection

- `redcap_version` - obtained from API call and affects links

- `last_sync` and `sync_frequency` - informs REDCap sync of when to
  update

- other variables from project info and some internal package mechanics

## See also

[`vignette("Cache", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Cache.md)

Other Cache Functions:
[`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md)
