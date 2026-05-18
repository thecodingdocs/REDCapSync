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
# put in RStudio console (ie bottom right panel and NOT IN A SCRIPT)
Sys.setenv(REDCAPSYNC_FIRST_PROJECT = "YoUrNevErShaReToken")
# or WAY better put this in your .Renviron file...
# REDCAPSYNC_FIRST_PROJECT = 'YoUrNevErShaReToken'
# Then save file, restart R session (`.rs.restartR()`) and library(REDCapSync)

# 2.) setting up a project -----------------------------------------------------
# help(setup_project)
DATA_STORED_HERE <- getwd() # choose appropriate/secure folder
# help(setup_project)
project <- setup_project(
  project_name = "FIRST_PROJECT", # default token name REDCAPSYNC_<project_name>
  redcap_uri = "https://redcap.fakei.edu/api/",   # your institution's link
  dir_path = DATA_STORED_HERE,    # choose appropriate/secure folder
  sync_frequency = "weekly", # default is "daily",
  get_entire_log = FALSE # TRUE for max summary, may be slow for huge projects
)
# install.packages("keyring") # if you dont have
project$test_token() # will have pop up for keyring if token fails

# 3.) running project$sync() ---------------------------------------------------
project$sync() # gets all data from REDCap based on setup
# help(project)
# project$url_launch() # opens REDCap in browser
project$generate_dataset("custom", envir = globalenv()) # identified
# add reusable datasets with project$add_dataset(...)
```

## Setup tokens

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
