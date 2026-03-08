# Clear your cached projects

Finds the location of the cache established by
[`hoard`](https://docs.ropensci.org/hoardr/reference/hoard.html) and
deletes stored project information (not data)! If you provide
`project_names`, it will remove only those projects from the cache.

## Usage

``` r
cache_clear(project_names = NULL)
```

## Arguments

- project_names:

  character vector of project project_names previously setup. If NULL,
  will get all from
  [`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)

## Value

message of outcome

## Details

The cache only stores information like project_name, token_name,
directory location, and other details from
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
If you want to truly delete the project files, you must do so at the
project directory you set up. See the
[`vignette("Cache", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Cache.md)
Vignette.
[`vignette("Cache", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Cache.md)
Or try
[[`vignette("Cache", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Cache.md)](https://thecodingdocs.github.io/REDCapSync/doc/Cache.md)
Or try Cache Or try
[REDCapSync_project](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync_project.md)
Or try Cache

## See also

[`vignette("Cache", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Cache.md)

Other Cache:
[`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)

## Examples

``` r
if (FALSE) { # \dontrun{
cache_clear("OLD_PROJECT")
cache_clear() # every project
} # }
```
