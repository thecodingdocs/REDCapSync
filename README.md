
## REDCapSync <a href="https://github.com/thecodingdocs/REDCapSync/"><img src="man/figures/logo.png" align="right" height="160" width="140"/></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/thecodingdocs/REDCapSync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thecodingdocs/REDCapSync/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/thecodingdocs/REDCapSync/graph/badge.svg)](https://app.codecov.io/gh/thecodingdocs/REDCapSync)

<!-- badges: end -->

Several R packages exist for using the
[REDCap](https://projectredcap.org/) Application Program Interface (API)
such as, [`redcapAPI`](https://github.com/vubiostat/redcapAPI),
[`REDCapR`](https://ouhscbbmc.github.io/REDCapR), and
[`tidyREDCap`](https://raymondbalise.github.io/tidyREDCap/). However,
[`REDCapSync`](https://thecodingdocs.github.io/REDCapSync) is the first
“get-everything” REDCap R package that converts REDCap projects into a
standardized, API-efficient, and project-agnostic \[R6\]\[R6::R6Class\]
object \[R6::R6Class\] \[`R6::R6Class`\] `R6::R6Class`.

## What is REDCapSync?

[`REDCapSync`](https://thecodingdocs.github.io/REDCapSync) unleashes the
full power of the REDCap API even for the basic R user. When a sync is
performed, [`REDCapSync`](https://thecodingdocs.github.io/REDCapSync)
uses a cache of previous saves, a user-defined directory, and the REDCap
log to only update data that changed since the last API call. Project
objects can be used for the best that R has to offer via statistics,
visualization, functions, shiny apps, and more!

The aims of [`REDCapSync`](https://thecodingdocs.github.io/REDCapSync)
are to…

1.  Encapsulate the REDCap API into one R6 object to streamline use.
2.  Automate common tasks such as cleaning, deidentification and merges.
3.  Automate distribution of user-defined Excel datasets to local/cloud
    storage for one or many REDCap projects.
4.  Convert ***uncoded*** REDCap data from R or Excel for upload using
    REDCap API.
5.  Power the companion shiny app
    [`RosyREDCap`](https://thecodingdocs.github.io/RosyREDCap/ "RosyREDCap R package")

By leveraging the combined strengths of R and REDCap, users can maintain
strong data pipelines that include statistics, visuals, and even shiny
applications!

## Installation

The stable *release* version can be installed from
[CRAN](https://cran.r-project.org/package=REDCapSync).

``` r
install.packages("REDCapSync")
```

### Devopment Version

You can install the development version from GitHub with the
[`pak`](https://pak.r-lib.org)

``` r
# install.packages("pak")
pak::pak("thecodingdocs/REDCapSync")
```

Alterntatively, you can install the development version from GitHub with
the [`remotes`](https://remotes.r-lib.org) package.

``` r
# install.packages("remotes")
remotes::install_github("thecodingdocs/REDCapSync")
```

Windows users may need to install [RTools version
4.5](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html "R Getting Started")
to use pak. If you have any issues, try downloading the most recent
version of R at RStudio and update all packages in RStudio. See
[thecodingdocs.com/r/getting-started](https://www.thecodingdocs.com/r/getting-started "R Getting Started").

## Getting Started!

Getting started is as simple as 1.) setting your token, 2.) setting up a
project, and 3.) running project\$sync(). See [Getting
Started](https://thecodingdocs.github.io/REDCapSync/articles/REDCapSync.html "Getting Started")
page for the basics!

![](man/figures/cover.jpg)

## Minimum Requirements

- R (and RStudio) installed on your computer or server.
- Access to at least one REDCap project (real or test) with API Token
  privileges according to user rights.
- Ideally, you should have User Permissions to logging in order to use
  the package effeciently
- Appropriate permission to export and analyze data for projects for
  which you have a token.
- Basic R knowledge such as installing a package and running code.
- Thoughtful attention to how and where data you create is used and
  stored.

## Disclaimers

- With great power comes great responsibility! The REDCap API has the
  with ability to read and write sensitive data. The API token holder is
  ultimately responsible for their activity and security.
- Always confirm that you have the appropriate permission to use and
  store data.
- REDCap is maintained by Vanderbilt University and is not responsible
  for testing, developing, or maintaining this software.
- This package is still in development and is subject to changes,
  especially pre-CRAN submission.

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
- Follow us on Twitter/X
  [x.com/TheCodingDocs](https://x.com/TheCodingDocs "TheCodingDocs Twitter")
- Follow me on Twitter/X
  [x.com/BRoseMDMPH](https://x.com/BRoseMDMPH "BRoseMDMPH Twitter")

[![TheCodingDocs.com](man/figures/TCD.png)](https://www.thecodingdocs.com)
