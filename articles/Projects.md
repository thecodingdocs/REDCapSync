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
several to build an environment. This allows `project$sync()` to update
`project` without having to do
`project <- project |> sync_something() |> another_function()`

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
#>  [1] "print"                        "sync"                        
#>  [3] "add_dataset"                  "add_field"                   
#>  [5] "add_custom_transformation"    "remove_added_fields"         
#>  [7] "remove_custom_transformation" "load_dataset"                
#>  [9] "remove_datasets"              "generate_dataset"            
#> [11] "save_datasets"                "save_dataset"                
#> [13] "save"                         "set_keyring_token"           
#> [15] "test_token"                   "url_launch"                  
#> [17] "url_record_launch"            "upload"
```
