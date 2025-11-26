# Get your Get Cache Path

Included for transparency and confirmation/testing. This is where the
basic information about your projects is cached when you use the
REDCapSync package.

## Usage

``` r
cache_path()
```

## Value

The file path of your REDCapSync cache

## Details

This function checks the location of the cache established by
[`hoard`](https://docs.ropensci.org/hoardr/reference/hoard.html). *No
project data is stored here. Tokens are not stored here either.* Key
information stored here is `project_name` (primary key for REDCapSync
projects) and other details about project information.

## See also

For more details, see
[`hoard`](https://docs.ropensci.org/hoardr/reference/hoard.html).

Other Project Cache Functions:
[`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md),
[`cache_remove_project()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_remove_project.md),
[`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- cache_path()
print(path)
} # }
```
