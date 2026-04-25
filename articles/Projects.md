# Projects

The goal of REDCapSync is to encapsulate a REDCap project into a
standardized object. R6 gives a way of keeping some elements internal to
the object. Behind-the-scenes is an R list where all of the metadata and
data is stored.

``` r
TEST_CLASSIC <- load_project(project_name = "TEST_CLASSIC")
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API
listviewer::jsonedit(TEST_CLASSIC$.internal)
```

The R6 object contains those things behind-the-scenes but then uses
several to build an environment. This allows `TEST$sync()` to update
`TEST` without having to do
`TEST <- TEST |> sync_something() |> another_function()`

``` r
project <- load_project("TEST_CLASSIC")
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API

# projects have read-only active bindings
names(REDCapSyncProject$active)
#> [1] "project_name" "dir_path"     "data"         "metadata"     "redcap"      
#> [6] ".internal"

# projects have public methods
names(REDCapSyncProject$public_methods) |> setdiff("initialize")
#>  [1] "print"             "sync"              "add_dataset"      
#>  [4] "load_dataset"      "remove_datasets"   "generate_dataset" 
#>  [7] "save_datasets"     "save_dataset"      "save"             
#> [10] "set_keyring_token" "test_token"        "url_launch"       
#> [13] "url_record_launch" "upload"
```
