# Remove project from cache

This will remove a project from the cache. The cache only stores
information like project_name, token_name, directory location, and
details from setup_project. If yo?u want to truly delete the project
files, you must do so at the project directory you set up.

## Usage

``` r
cache_remove_project(project_name)
```

## Arguments

- project_name:

  A character string with no spaces or symbols representing the unique
  short name for the REDCap project.

## Value

message of outcome

## See also

Other Project Cache Functions:
[`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md),
[`cache_path()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_path.md),
[`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)
