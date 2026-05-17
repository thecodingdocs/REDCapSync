# Get Started

## Setup

First load the namespace and check your saved projects.

``` r

library(REDCapSync)

projects$print()

View(projects$df())                   # Vew your previously saved projects
```

You can set your REDCap token in three ways! If you need more help
setting your tokens, See the
[Tokens](https://thecodingdocs.github.io/REDCapSync/articles/vignette(%22Tokens%22,%20package%20=%20%22REDCapSync%22))
vignette.

``` r
#1. set in your Renviron file
usethis::edit_r_environ()                                 #finds your file
# another way to find same file
rstudioapi::navigateToFile(file.path(Sys.getenv("HOME")), ".Renviron"))
# Then you add --> `REDCAPSYNC_FIRST_PROJECT = 'YoUrNevErShaReToken'`
# then save file and restart R session (`.rs.restartR()`) and reload package

#2. set to your token with keyring package! (make sure is installed)
project$set_keyring_token() # now enter token in pop-up

#3. set each time in your console or script (not recommended!) RISKY
Sys.setenv(REDCAPSYNC_FIRST_PROJECT = "YoUrNevErShaReToken")

# Confirm
# If it worked you will see your token when you run...
Sys.getenv("REDCAPSYNC_FIRST_PROJECT") # for R eviron
# or...
keyring::key_get(service = config$keyring.service(), # for keyring
                 username = "FIRST_PROJECT", # is project_name
                 keyring = config$keyring())
```

## Run Core Functions

The following functions represent the core functions of the package.

``` r

project <- setup_project(
  project_name = "FIRST_PROJECT", # default token name REDCAPSYNC_<project_name>
  redcap_uri = "https://redcap.fakei.edu/api/",        # your institution's link
  dir_path = getwd()     # your project file path, real data will be stored here
)
project$sync() # gets all data from REDCap
```

## Explore Outputs!

``` r

fields <- project$metadata$fields            # adds fields in global environment
choices <- project$metadata$choices         # adds choices in global environment
project$data |> list2env(envir = globalenv())  # adds data forms in global envir
```

Take note you are assigning variables above. So if you have a redcap
form called “project”, “fields”, “choices”, etc. you will have to adjust
what you choose to call them in your script to avoid any conflicts.

## Go Further by Generating Datasets!

This is the intended way to extract data from the project object. It has
parameters that control what is exported. It is unique from simply
exporting the raw data because it can summarize data from the log,
change the columns to R variable types, add variables, etc. It can also
be used for visualization functions from
[`RosyREDCap`](https://thecodingdocs.github.io/RosyREDCap/ "RosyREDCap R package").

The following default merges all non-repeating forms and adds them to
the right of any repeating forms. It also uses the REDCap log and REDCap
data to add key information to metadata, users, and records.

``` r

project$load_dataset(summary_name = "REDCapSync", envir = globalenv())
```

This can be customized with `add_dataset` for refreshing datasets or
`generate_dataset` for R-only use. This can give you filtered data (and
corresponding annotations to metadata, etc), deidentified data, and
more!

``` r
project$generate_dataset(
  dataset_name = "custom_name"
  filter_field = "has_something"  # Raw REDCap variable name
  filter_choices = "Yes"          # REDCap labelled choice
  exclude_identifiers = TRUE,
  exclude_free_text = TRUE,
  include_metadata = TRUE,
  envir = globalenv()
)
```

For an in-depth demonstration of both REDCapSync and RosyREDCap, see
[thecodingdocs.github.io/RMed26-Demo](https://thecodingdocs.github.io/RMed26-Demo/ "RMed26-Demo").
