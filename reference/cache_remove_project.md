# Remove project from cache

This will remove a project from cache. Remember cache only stores
information like short_name, token_name, directory location, and more
from setup_project. If you want to truly delete the project files go the
folder you setup.

## Usage

``` r
cache_remove_project(short_name)
```

## Arguments

- short_name:

  A character string with no spaces or symbols representing the unique
  short name for the REDCap project.

## Value

message of outcome

## See also

Other Project Cache Functions:
[`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md),
[`cache_path()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_path.md),
[`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)
