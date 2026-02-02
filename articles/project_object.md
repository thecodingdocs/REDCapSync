# The REDCapSync_project Object

The goal of REDCapSync is to encapsulate a REDCap project into a
standardized object. R6 gives a way of keeping some elements internal to
the object. Behind-the-scenes is an R list where all of the metadata and
data is stored.

``` r
TEST_CLASSIC <- load_test_project(project_name = "TEST_CLASSIC")
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API
listviewer::jsonedit(TEST_CLASSIC$.internal)
```

The R6 object contains those things behind-the-scenes but then uses
several to build an environment. This allows `TEST$sync()` to update
`TEST` without having to do
`TEST <- TEST |> sync_something() |> another_function()`

``` r
library(REDCapSync)
TEST <- load_test_project()
# Update From REDCap 

# Explore Whats Inside

# Generate Outputs
```
