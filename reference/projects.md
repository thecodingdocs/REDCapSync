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

A named list of functions for managing REDCapSync projects.

## Details

`projects` is implemented as a singleton list object and serves as a
single entry point to the REDCapSync workflow. All project-level
operations—such as setup, loading, syncing, and removal—are accessed
through this object.

A key advantage of this design is support for **method chaining**, even
without loading the namespace. Because many project methods return
project objects, you can write concise, readable workflows that operate
in sequence:


    # load, sync, and generate dataset to global
    REDCapSync::projects$load("TEST_CLASSIC")$
      sync()$
      generate_dataset(envir = globalenv(),
                       filter_field = "var_branching",
                       filter_choices = "Yes")

## Methods

- [`print()`](https://rdrr.io/r/base/print.html):

  Display a summary of all registered projects.

  - Shows total number of projects

  - Lists project names

  - Indicates how many are due for sync

  Returns the project data frame invisibly.

- [`any()`](https://rdrr.io/r/base/any.html):

  Check whether any projects are registered.

  Returns a logical scalar.

- `n()`:

  Return the number of registered projects.

  Returns an integer.

- [`names()`](https://rdrr.io/r/base/names.html):

  Return the names of registered projects.

  Returns a character vector.

- `test_names()`:

  Return the names of test projects.

  Returns a character vector.

- [`df()`](https://rdrr.io/r/stats/Fdist.html):

  Retrieve the project registry as a data frame.

  Returns a data frame containing stored project metadata.

- `load(project_name)`:

  Load a project by name. This is a wrapper for
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

  **Arguments**

  - `project_name`: Character. Name of the project.

  Returns a `REDCapSyncProject` object.

## See also

[`vignette("REDCapSync", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/REDCapSync.md)
[`vignette("Cache", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Cache.md)
[`vignette("RosyREDCap", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/RosyREDCap.md)
[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing projects
[project](https://thecodingdocs.github.io/REDCapSync/reference/project.md)
for using the project objects

## Examples

``` r
project_df <- projects$df()
#> ! No cached projects... use `setup_project(...)`
projects$print()
#> ! No cached projects... use `setup_project(...)`
#> 
#> ── REDCapSync ──────────────────────────────────────────────────────────────────
#> 
#> ── Your Projects (n = 0) ──
#> 
#> ── Test Projects (n = 10) ──
#> 
#> TEST_CLASSIC, TEST_REPEATING, TEST_LONGITUDINAL, TEST_MULTIARM, TEST_EDGE,
#> TEST_DATA, TEST_CANCER, TEST_REDCAPR_SIMPLE, TEST_REDCAPR_LONGITUDINAL,
#> TEST_REDCAPR_CLIN_TRIAL
#> 
#> ── Help ──
#> 
#> Pkgdown: <https://thecodingdocs.github.io/REDCapSync/>
#> Github: <https://github.com/thecodingdocs/REDCapSync/>
#> Datasets: `vignette(REDCapSync::Datasets)`
#> Tokens: `vignette(REDCapSync::Tokens)`
projects$any()
#> ! No cached projects... use `setup_project(...)`
#> [1] FALSE
projects$n()
#> ! No cached projects... use `setup_project(...)`
#> [1] 0
projects$names()
#> ! No cached projects... use `setup_project(...)`
#> character(0)
project <- projects$load("TEST_CLASSIC")
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API
```
