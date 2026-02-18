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
Sys.setenv(REDCAPSYNC_TEST1 = "YoUrNevErShaReToken")
#2. set to your private R sessions!
usethis::edit_r_environ()                                       #finds your file
# or
rstudioapi::navigateToFile(file.path(Sys.getenv("HOME")), ".Renviron"))
# Then you add --> REDCAPSYNC_TEST1 = 'YoUrNevErShaReToken'
# then save file and restart R
# If it worked you will see your token when you run...
Sys.getenv("REDCAPSYNC_TEST1")
```

## Run Core Functions

The following functions represent the core functions of the package.

``` r
project <- setup_project(
  project_name = "TEST1",          # must match token pattern REDCAPSYNC_*******
  redcap_uri = "https://redcap.fakei.edu/api/",        # your institution's link
  dir_path = getwd()     # your project file path, real data will be stored here
)
project$sync() # gets all data from REDCap
```

## Explore Outputs!

``` r
fields <- project$metadata$fields            # adds fields in global environment
choices <- project$metadata$choices         # adds choices in global environment
project$data |> 
  list2env(envir = globalenv())          # adds data forms in global environment
```

Take note you are assigning variables above. So if you have a redcap
form called “project”, “fields”, “choices”, etc. you will have to adjust
what you choose to call them in your script to avoid any conflicts.

## Go Further by Generating Summaries!

This is the intended way to extract data from the project object. It has
parameters that control what is exported. It is unique from simply
exporting the raw data because it can summarize data from the log,
change the columns to R variable types, add variables, etc. It can also
be used for visualization functions from
[RosyREDCap](https://thecodingdocs.github.io/REDCapSync/articles/).

The following default merges all non-repeating forms and adds them to
the right of any repeating forms. It also uses the REDCap log and REDCap
data to add key information to metadata, users, and records.

``` r
project$generate_summary(
  summary_name = "REDCapSync",
  envir = globalenv()
  )
```

This can be customized with `add_summary` for refreshing datasets or
`generate_summary` for R-only use. This can give you filtered data (and
corresponding annotations to metadata, etc), de-identified data, and
more!

``` r
project$generate_summary(
    filter_field = "has_something"  # Raw REDCap variable name
    filter_choices = "Yes"          # REDCap labelled choice
    exclude_identifiers = TRUE,
    exclude_free_text = TRUE,
    include_metadata = TRUE,
    envir = globalenv()
  )
```
