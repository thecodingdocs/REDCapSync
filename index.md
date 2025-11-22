## REDCapSync [![](reference/figures/logo.png)](https://github.com/thecodingdocs/REDCapSync/)

Several R packages exist for using the REDCap Application Program
Interface (API) such as,
[REDCapR](https://ouhscbbmc.github.io/REDCapR/ "REDCapR R package"),
[redcapAPI](https://github.com/vubiostat/redcapAPI/ "redcapAPI R package"),
and
[tidyREDCap](https://raymondbalise.github.io/tidyREDCap/ "redcapAPI R package").
However, there is no “get-everything-from-REDCap” R package that can
produce a standardized R object for any REDCap project.
[REDCapSync](https://github.com/thecodingdocs/REDCapSync) streamlines
comprehensive extraction with two key functions:
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
and `sync_project()`. When a sync is performed
[REDCapSync](https://github.com/thecodingdocs/REDCapSync) uses the
REDCap log to only update data that has changed since the last API call.
The final project-agnostic object facilitates further pipelining for
deidenification, deriving additional variables, linked excel sheets, and
exploratory data analysis with
[RosyREDCap](https://thecodingdocs.github.io/RosyREDCap/ "RosyREDCap").

## What is `{REDCapSync}`?

Using a cache of previous saves, a file directory, and the REDCap log,
{REDCapSync} updates only the data that has been changed since the last
API call. Each project becomes a standardized nested R list object that
can be used for the best that R has to offer via statistics,
visualization, shiny apps, and more! REDCapSync unleashes the full power
of the REDCap API even for the basic R user.

The aims of [REDCapSync](https://github.com/thecodingdocs/REDCapSync)
are to…

1.  Wrap the REDCap API functionality “behind-the-scenes” to streamline
    its use.
2.  Maintain local/cloud versions of one or many REDCap projects by only
    updating recently changed records on a user-defined schedule, such
    as daily, weekly, or monthly.
3.  Automate tasks such as cleaning, deidentification and standard
    transformations.
4.  Allow bulk exports AND imports of ***uncoded*** REDCap data using R
    and Excel.
5.  Power the companion shiny app
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

![](reference/figures/cover.jpg)

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
- Follow us on Twitter
  [twitter.com/TheCodingDocs](https://twitter.com/TheCodingDocs "TheCodingDocs Twitter")
- Follow me on Twitter
  [twitter.com/BRoseMDMPH](https://twitter.com/BRoseMDMPH "BRoseMDMPH Twitter")

[![TheCodingDocs.com](reference/figures/TCD.png)](https://www.thecodingdocs.com)
