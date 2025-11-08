# Clear your cached projects

Included for transparency and confirmation/testing.

## Usage

``` r
cache_clear()
```

## Value

messages confirming deleted cache

## Details

This function checks the location of the cache established by
[`hoard`](https://docs.ropensci.org/hoardr/reference/hoard.html) and
deletes it! This will not delete project data, just the packages stored
"memory" of it.

## See also

Other Project Cache Functions:
[`cache_path()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_path.md),
[`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)
