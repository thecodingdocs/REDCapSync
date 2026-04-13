# Datasets

``` r
library(REDCapSync)
```

Start by loading an exisiting or test project. There are two named
datasets by default: REDCapSync and REDCapSync_raw.

``` r
project <- load_project(project_name = "TEST_CLASSIC")

dataset <- project$load_dataset("REDCapSync")
```
