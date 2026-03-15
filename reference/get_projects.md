# Get your REDCap projects used by REDCapSync

Everytime a project is synced, basic project information is saved to the
package user cache so that
[load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
and [sync](https://thecodingdocs.github.io/REDCapSync/reference/sync.md)
work across R sessions.

## Usage

``` r
get_projects()
```

## Value

data.frame of cached projects

## Details

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
