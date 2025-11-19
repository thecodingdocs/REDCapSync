# Get REDCap Report

Get REDCap Report

## Usage

``` r
get_redcap_report(project, report_id, silent = TRUE)
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

- report_id:

  character or integer of REDCap report ID. This can be found at the end
  of the URL of the report.

- silent:

  Logical (TRUE/FALSE). For messages.

## Value

data.frame of REDCap report
