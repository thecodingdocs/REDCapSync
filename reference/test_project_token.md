# Test REDCap API Token linked to a project Object

Validates the REDCap API token stored in the `project` object by
attempting a connection to the REDCap server.

## Usage

``` r
test_project_token(project)
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

## Value

Logical. Returns `TRUE` if the API token is valid, otherwise `FALSE`.

## Details

This function tests whether the API token stored in the `project` object
is valid by making a request to the REDCap server.

## See also

[pkgdown article on
tokens](https://thecodingdocs.github.io/REDCapSync/articles/Tokens.md)
[pkgdown article on
tokens](https://thecodingdocs.github.io/REDCapSync/articles/Tokens.html)

Other Token Functions:
[`view_project_token()`](https://thecodingdocs.github.io/REDCapSync/reference/view_project_token.md)
