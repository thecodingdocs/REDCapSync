# View the REDCap API Token Stored in the Session

Displays the REDCap API token currently stored in the session as an
environment variable. It's essentially a wrapper for
Sys.getenv("YOUR_TOKEN_NAME"), but it also validates that the token is
formatted like a REDCap token and provides messgaes if not valid.

## Usage

``` r
view_project_token(project)
```

## Arguments

- project:

  A validated `project` object containing REDCap project data and
  settings. Generated using
  [load_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  or
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

## Value

Invisible. Prints a message displaying the stored token.

## Details

This function retrieves the REDCap API token associated with the
specified `project` object and displays it as a message. The token is
not returned as an R object to maintain security. Use this function to
confirm the token currently in use without exposing it unnecessarily.

## See also

Other Token Functions:
[`test_project_token()`](https://thecodingdocs.github.io/REDCapSync/reference/test_project_token.md)
