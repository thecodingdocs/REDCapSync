# Get your REDCap projects used by REDCapSync

Everytime a setup or update is performed REDCapSync stores the most
basic information about that project to the cache so the user has a
running log of everywhere there project information is stored, which can
be used to find, move, edit, delete that data.

## Usage

``` r
get_projects()
```

## Value

data.frame of projects from the cache

## See also

Other Project Cache Functions:
[`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md),
[`cache_path()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_path.md),
[`cache_remove_project()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_remove_project.md)
