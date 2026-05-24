# Get Started

First load the namespace and check your saved projects.

``` r

library(REDCapSync)
projects$print()
# View(projects$df())                   # Vew your previously saved projects
```

## Very Quick Guide

Getting started is as simple as 1.) setting your token, 2.) setting up a
project, and 3.) running project\$sync().

``` r

# 1.) setting your token -------------------------------------------------------
Sys.setenv(REDCAPSYNC_FIRST_PROJECT = "YoUrNevErShaReToken") # in console
# or WAY BETTER put this in your .Renviron file...
# REDCAPSYNC_FIRST_PROJECT = 'YoUrNevErShaReToken'
# Then save file, restart R session (`.rs.restartR()`) and library(REDCapSync)

# 2.) setting up a project -----------------------------------------------------
project <- setup_project(
  project_name = "FIRST_PROJECT",                       
  redcap_uri = "https://redcap.fake.edu/api/",             # same as REDCapR
  dir_path = getwd(),                            # choose appropriate folder
  sync_frequency = "daily",                          # only checks max daily 
  get_entire_log = TRUE                       # for small or medium projects
)

# install.packages("keyring") 
project$test_token()                    # will launch keyring if token fails

# 3.) running project$sync() ---------------------------------------------------
project$sync() 

project$generate_dataset("custom", envir = globalenv())       
```

## Setup tokens

You can set your REDCap token in three ways! If you need more help
setting your tokens, see the
[Tokens](https://thecodingdocs.github.io/REDCapSync/articles/Tokens.md "Tokens vignette.")
vignette.

``` r
#1. set in your Renviron file
usethis::edit_r_environ()                                 #finds your file
# another way to find same file
rstudioapi::navigateToFile(file.path(Sys.getenv("HOME")), ".Renviron"))
# Then you add --> `REDCAPSYNC_FIRST_PROJECT = 'YoUrNevErShaReToken'`
# then save file and restart R session (`.rs.restartR()`) and reload package

#2. set to your token with keyring package! (make sure is installed)
keyring::key_set(service = config$keyring.service(), # for keyring
                 username = "FIRST_PROJECT", # is project_name
                 keyring = config$keyring()) # now enter token in pop-up
# ... or if have project object already
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

## Explore Outputs!

``` r

# raw REDCap -------------------------------------------------------------------
forms <- project$metadata$forms 
fields <- project$metadata$fields 
choices <- project$metadata$choices 
log <- project$redcap$log
users <- project$redcap$users
form1 <- project$data$form1
form2 <- project$data$form2
# project$data |> list2env(globalenv()) # all forms to global

# summarized REDCap ------------------------------------------------------------
# has added columns using log to annotate metadata and users etc. customizable
# help(dataset)
# dataset <- project$load_dataset("REDCapSync") # de-identified and no free text
dataset <- project$generate_dataset("custom") # identified (see parameters)
annotated_forms <- dataset$metadata$forms 
annotated_fields <- dataset$metadata$fields 
annotated_choices <- dataset$metadata$choices 
annotated_users <- dataset$users
merged <- dataset$data$merged
# dataset$data |> list2env(globalenv()) # all forms to global
# dataset$to_envir(globalenv()) # send to R global env
# can modify if you want before save (the actual dataset object not global)
# dataset$save() # will save to output folder by default
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
data to add key information to metadata, users, and records. By default
it is deidentified based on REDCap settings and excludes free text (but
not dates).

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

See the
[Datasets](https://thecodingdocs.github.io/REDCapSync/articles/Datasets.md "Datasets vignette")
vignette.

## Framework

![](images/framework.png)

For an in-depth demonstration of both REDCapSync and RosyREDCap, see
[thecodingdocs.github.io/RMed26-Demo](https://thecodingdocs.github.io/RMed26-Demo/ "RMed26-Demo").
