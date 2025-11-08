# Get Started

## Setup

This is how you get REDCap turned into an R database…

``` r
library(REDCapSync)

projects <- get_projects()                         # get list of cached projects

View(projects)                             # show your previously saved projects
```

You can set your REDCap token in two ways! If you need more help setting
your tokens see this article –\> placeholder

``` r
#1. set each time in your session (not recommended in saved/shared scripts!)
Sys.setenv(REDCapSync_TEST1 = "YoUrNevErShaReToken")
#2. set to your private R sessions!
usethis::edit_r_environ()                                       #finds your file
# Then you add --> REDCapSync_TEST = 'YoUrNevErShaReToken'
# then save file and restart R
# If it worked you will see your token when you run...
Sys.getenv("REDCapSync_TEST1")
```

## Run Core Functions

The following functions represent the core functions of the package.

``` r
project <- setup_project(
  short_name = "TEST1",                #must match token pattern REDCapSync_**** 
  redcap_uri = "https://redcap.miami.edu/api/",  # change to your institutions link
  dir_path = getwd(),                     # or change to your intended file path
)
project <- sync_project(project) 
```

## Explore Outputs!

``` r
project$metadata %>% list2env(envir = globalenv())
project$data %>% list2env(envir = globalenv())
```

## Go Further by Generating Summaries!

The following default merges all non-repeating forms and adds them to
the right of any repeating forms. It also uses the REDCap log and REDCap
data to add key information to metadata, users, and records.

``` r
project %>% 
  generate_project_summary(summary_name = "REDCapSync") %>% 
  list2env(envir = globalenv())
```

This can be customized with `add_project_summary` for refreshing
datasets or `generate_project_summary` for R-only use. This can give you
filtered data (and corresponding annotations to metadata, etc),
de-identified data, and more!

``` r
project %>% 
  generate_project_summary(
    filter_field = "has_feature" # Raw REDCap variable name
    filter_choices = "Yes"       # REDCap Labelled choice,
    exclude_identifiers = TRUE,
    exclude_free_text = TRUE,
    include_metadata = TRUE
  ) %>% 
  list2env(envir = globalenv())
```
