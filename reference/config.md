# Package configuration accessors

Internal configuration helpers used to retrieve package configuration
values from options or environment variables.

## Usage

``` r
config
```

## Format

An object of class `list` of length 6.

## Details

Configuration is resolved in the following order:

1.  `options("redcapsync.config.*")`

2.  `REDCAPSYNC_CONFIG_*` for logical and filepath types

3.  Function default

The `config` object is a list of accessor functions that retrieve
configuration values and validate them.

- allow.test.names:

  Logical. Whether
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  allows project names starting with `"TEST_"`.

- show.api.messages:

  Logical. Whether to display API messages returned by REDCapR.

- verbose:

  Logical. Controls verbosity of package messages.

- cache.dir:

  Character file path overriding the default cache directory.

- header.style:

  `openxlsx` style object used for Excel header formatting.

- body.style:

  `openxlsx` style object used for Excel body formatting.
