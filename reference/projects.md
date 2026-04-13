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

An object of class `list` of length 5.

## Value

list of functions used as single entry into REDCapSync package

## Details

`projects` is implemented as a singleton list object and serves as a
single entry point to the REDCapSync workflow. All project-level
operations—such as setup, loading, syncing, and removal—are accessed
through this object.

A key advantage of this design is support for **method chaining**.
Because many project methods return project objects, you can write
concise, readable workflows that operate in sequence:

    # load previous project and sync with API
    project <- REDCapSync::projects$load("TEST_CLASSIC")$sync()
    # load dataset to global envir based on custom filters
    dataset <- project$generate_dataset(envir = globalenv(),
                                        filter_field = "var_branching",
                                        filter_choices = "Yes")

## See also

[`vignette("Cache", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Cache.md)

Other Cache Functions:
[`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md)
