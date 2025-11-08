# Upload to REDCap

This will only overwrite and new data. It will not directly delete and
data. Because this is the only function that can mess up your data, use
it at your own risk. Remember all changes are saved in the redcap log if
there's an issue. Missing rows and columns are fine!

## Usage

``` r
upload_form_to_REDCap(to_be_uploaded, project, batch_size = 500)
```

## Arguments

- to_be_uploaded:

  data.frame in raw coded form. If you worked with clean data pass your
  data to `labelled_to_raw_form(form,project)` first.

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

- batch_size:

  numeric of how big the REDCap batch upload is. Default 500.

## Value

messages
