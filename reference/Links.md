# Open Links to REDCap Pages

Opens browser page for a given project object.

## Usage

``` r
link_API_token(project)

link_API_playground(project)

link_REDCap_home(project)

link_REDCap_project(project)

link_REDCap_record(project, record, page, instance, text_only = FALSE)
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  `project <- `[`load_project`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)`("PROJ")`
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)()

- record:

  REDCap record id or study id etc, any column names that match
  `project$metadata$id_col`

- page:

  REDCap page for the record. Must be one of
  `project$metadata$forms$form_name`

- instance:

  REDCap instance if it's a repeating instrument

- text_only:

  logical for only returning text

## Value

Nothing will be returned in R. Instead, a browser link

## Details

Uses [`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html) to
open the specified REDCap page. In order for the function to work you
must have ran `project <- sync_project(project)` successfully at least
once. If the link brings you to a page that doesn't work check the URL.
It's possible your institution may have changed redcap versions, which
is part of the URL. In that case run `project <- sync_project(project)`
again. You may have to be signed into REDCap for it to work properly.
When in doubt, just seek out the page by navigating on your own in
REDCap. Report issues if you can.
