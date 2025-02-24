
## REDCapSync <a href="https://github.com/thecodingdocs/REDCapSync/"><img src="man/figures/logo.png" align="right" height="160" width="140"/></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/thecodingdocs/REDCapSync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thecodingdocs/REDCapSync/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/thecodingdocs/REDCapSync/graph/badge.svg)](https://app.codecov.io/gh/thecodingdocs/REDCapSync)

<!-- badges: end -->

Several R packages exist for using the REDCap Application Program
Interface (API) such as,
[REDCapR](https://ouhscbbmc.github.io/REDCapR/ "REDCapR R package"),
[redcapAPI](https://github.com/vubiostat/redcapAPI/ "redcapAPI R package"),
and
[tidyREDCap](https://raymondbalise.github.io/tidyREDCap/ "redcapAPI R package").
However, there is no R package for maintaining synchronized data
pipelines from REDCap. Enter `{REDCapSync}`, which streamlines
comprehensive extraction from one or multiples REDCap projects with
`REDCapSync::sync()`.

## What is `{REDCapSync}`?

Using a cache of previous saves, a file directory, and the REDCap log,
{REDCapSync} updates only the data that has been changed since the last
call. Each project becomes a standardized nested R list object that can
be used for the best that R has to offer via statistics, visualization,
shiny apps, and more! REDCapSync unleashes the full power of the REDCap
API even for the basic R user.

The evolving aims of `{REDCapSync}` are to…

1.  Wrap the REDCap API functionality “behind-the-scenes” to streamline
    its use.
2.  Maintain local/cloud versions of one or many REDCap projects by only
    updating recently changed records on a user-defined schedule, such
    as daily, weekly, or monthly.
3.  Automate standard tasks such as cleaning, deidentification and
    quality control.
4.  Standardize creation of custom data transformations and subsets that
    are passed down to a directory.
5.  Allow bulk imports of ***uncoded*** data using R or Excel/CSV.
6.  Power the companion shiny app
    [RosyREDCap](https://thecodingdocs.github.io/RosyREDCap/ "RosyREDCap R package")

By leveraging the combined strengths of R and REDCap, users can maintain
strong data pipelines that include statistics, visuals, and even shiny
applications!

## Installation

The stable release can be found on CRAN (PENDING SUBMISSION) and
installed with:

``` r
#install.packages("REDCapSync") #PLACEHOLDER NOT SUBMITTED TO CRAN YET
```

### Devopment Version

You can install the development version of REDCapSync from GitHub by
using the `pak` package.

``` r
# install.packages("pak")
pak::pak("thecodingdocs/REDCapSync")
```

If you have any issues, try downloading the most recent version of R at
RStudio and update all packages in RStudio. See
[thecodingdocs.com/r/getting-started](https://www.thecodingdocs.com/r/getting-started "R Getting Started").

### Getting Started!

Getting started is as simple as 1.) setting your tokens, 2.) setting up
a project, and 3.) running sync. See [Getting
Started](https://thecodingdocs.github.io/REDCapSync/articles/REDCapSync.html "Getting Started")
page for the basics!

… If you want to be guided through your first setup, simply run the
following!

``` r
REDCapSync::sync()
```

![](man/figures/cover.jpg)

## Minimum Requirements

- R (and RStudio) installed on your computer or server.
- Access to at least one REDCap project (real or test) with API Token
  privileges according to user rights.
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
- Follow us on Twitter
  [twitter.com/TheCodingDocs](https://twitter.com/TheCodingDocs "TheCodingDocs Twitter")
- Follow me on Twitter
  [twitter.com/BRoseMDMPH](https://twitter.com/BRoseMDMPH "BRoseMDMPH Twitter")

[![TheCodingDocs.com](man/figures/TCD.png)](https://www.thecodingdocs.com)
