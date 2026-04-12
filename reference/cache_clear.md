# Clear your cached projects

Finds the location of the cache established by
[`hoard`](https://docs.ropensci.org/hoardr/reference/hoard.html) and
deletes stored project information (not data)! If you provide
`project_names`, it will remove only those projects from the cache. If
you want to truly delete the project files, you must do so at the
project directory you set up.

## Usage

``` r
cache_clear(project_names = NULL)
```

## Arguments

- project_names:

  character vector of project project_names previously setup. If NULL,
  will get all from `get_projects()`

## Value

Message of outcome and invisible NULL.

## Details

The cache only stores information like project_name, token_name,
directory location, and other details from
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).The
default location of the cache location is defined by using
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
[`projects`](https://thecodingdocs.github.io/REDCapSync/reference/projects.md)

## Examples

``` r
if (FALSE) { # \dontrun{
cache_clear("OLD_PROJECT")
cache_clear() # every project
} # }
```
