# REDCapSync 0.1.1.9000 (development version)

* Preparing for R journal submission
* Waiting for suggestions/issues
* Preparing RosyREDCap for CRAN submission

## New Features

* New add_fields feature for derived fields and recalculating existing fields 
* New remove_added_fields feature to remove anything from add_fields section

## Internal Changes

* reconcile_version function will refresh datasets if package version changes
* Excel sheets will have filters by default

# REDCapSync 0.1.1

## Fixes

* CRAN Policy fix that macOS requires "/Library/Caches/org.R-project.R/R" not 
"/Library/Caches/R/REDCapSync"
* Fixed bad links for repeating instruments
* Fixed incorrect log labeling for Create Response and Update Response
* Accounting for renamed records in sync and previous log
* Renamed records will no longer trigger full update

## Internal Changes

* Datasets now have a preview file function `dataset$preview()`
* switched to openxlsx2 internally for sustainability
* config (experimental) accounts for new xlsx options

***************************************************************************

# REDCapSync 0.1.0

## Initial CRAN submission!

* Tested on over 30 projects at least 4 institutions
* Presented at R Medicine Conferences 2024, 2025, and 2026
* Discussed at RAISE meeting in context of other REDCap API packages
* Integrated REDCapR and redcapAPI for API functions
* Committed to R6 for project object
* Developed dataset object
* Separated out RosyREDCap shiny app
