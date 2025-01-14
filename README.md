
# REDCapSync <img src="man/figures/logo.png" align="right" height="150" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/thecodingdocs/REDCapSync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thecodingdocs/REDCapSync/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/thecodingdocs/REDCapSync/graph/badge.svg)](https://app.codecov.io/gh/thecodingdocs/REDCapSync)
<!-- badges: end -->

Use your REDCap API tokens to create an updating R list object (DB)
which can used downstream for analysis, data exports, shiny apps, and
data imports! Rosy with a capital ‘R’ evokes the R statistical
programming language, the primary developer’s last name, and the idea of
making something more beautiful and user-friendly. This prefix combined
with REDCap demonstrates the package’s goal of enhancing the REDCap
experience by creating data tools that are powerful and pleasant to use,
combining the best of R with best REDCap.

At this time this package is **not suited for Multi-Arm projects or
massive REDCap projects yet**. More to come in future versions! The core
functions are stable but future development and changes are possible.

## Minimum Requirements

- R (and ideally RStudio) installed on your computer.
- Access to at least one REDCap project (real or test) with API Token
  Privileges according to User Rights.
- Permission to export and analyze date for this project.

![](man/figures/cover.jpg)

# What is `{REDCapSync}`?

R and REDCap are both widely utilized in medicine, including basic
science, clinical research, and clinal trials. Both have independent
strengths, but together they can create powerful data pipelines. While
several R packages exist for extracting data using the REDCap API,
`{REDCapSync}` stands out by offering comprehensive extraction of all
metadata and data from any REDCap project into a standardized R list
object, facilitating effortless downstream use in exports, imports,
transformation, and applications. This is showcased in the exploratory
data analysis shiny app included in the package. The three core aims of
`{REDCapSync}` are to…

1.  Maintain a local version of the REDCap project by only calling
    recently updated records using the REDCap log.
2.  Allow imports of non-coded versions of the dataset using R or
    Excel/CSV.
3.  Launch a shiny app that allows you to explore all of your REDCap
    projects.

By leveraging the combined strengths of R and REDCap, users can maintain
strong clinical data pipelines, collected and processed appropriately to
improve research and patient care. REDCapSync can be used as a base data
object and data quality tool for most REDCap projects to aid in
collection, monitoring, transformation, and analysis.

## Installing REDCapSync

The stable release can be found on CRAN and installed with:
**PLACEHOLDER NOT SUBMITTED TO CRAN YET**

``` r
#install.packages("REDCapSync") #PLACEHOLDER NOT SUBMITTED TO CRAN YET
```

You can install the development version of REDCapSync from GitHub by
using the `{remotes}` package. Be sure to install `{remotes}` if you
don’t have it already.

``` r
#install.packages("remotes)
remotes::install_github("thecodingdocs/REDCapSync",build_vignettes = T,build_manual = T)
```

Note that the version used at the time of writing this book is
0.0.0.9003. You can check what version you have installed with the
following.

``` r
packageVersion("REDCapSync")
#> [1] '0.0.0.9003'
```

If you have any issues, try downloading the most recent version of R at
RStudtio and update all packages in RStudio. See
[thecodingdocs.com/r/getting-started](https://www.thecodingdocs.com/r/getting-started "R Getting Started").

## Getting Started

``` r
library("REDCapSync")

projects <- get_projects() # get list of cached projects
print.data.frame(projects) # show your previously saved projects

your_project_path <- getwd()
print(your_project_path) # confirm this is where you want files saved

TEST <- setup_project(
  short_name = "TEST_repeating",
  redcap_base = "https://redcap.yourinstitution.edu/", # change to your institutions link (stop at ".edu/")
  dir_path = getwd() # or change to your intended file path
) #defaults will autocheck for valid token or ask you.

#if you had to set a token above you might need to select the code below again for it to run
TEST <- sync_project(TEST) # update from redcap by checking log and using saved object 
```

## Links

- The REDCapSync package is at
  [github.com/thecodingdocs/REDCapSync](https://github.com/thecodingdocs/REDCapSync "REDCapSync R package").
  See instructions above.
- The RosyREDCap package is at
  [github.com/thecodingdocs/RosyREDCap](https://github.com/thecodingdocs/RosyREDCap "RosyREDCap R package").
  See instructions above.
- Donate if I helped you out and want more development (anything helps)!
  [account.venmo.com/u/brandonerose](https://account.venmo.com/u/brandonerose "Venmo Donation")
- For more R coding visit
  [TheCodingDocs.com](https://www.thecodingdocs.com/ "TheCodingDocs.com")
- For correspondence/feedback/issues, please email
  <TheCodingDocs@gmail.com>!
- Follow us on Twitter
  [twitter.com/TheCodingDocs](https://twitter.com/TheCodingDocs "TheCodingDocs Twitter")
- Follow me on Twitter
  [twitter.com/BRoseMDMPH](https://twitter.com/BRoseMDMPH "BRoseMDMPH Twitter")

[![TheCodingDocs.com](man/figures/TCD.png)](http://www.thecodingdocs.com)
