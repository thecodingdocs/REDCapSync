# Configuration

Configuration can be set with options as well as environment variables

allow.test.names is a logical for if
[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
will allow projects that start with "TEST\_", which in general will be
reserved for test fixture data. If the user wants to override this they
can.

show.api.messages is a logical for showing REDCapR messages

verbose is logical for how many messages to see in general

cache.dir is a character file path if user wants to override cache
location.

header.style is a openxlsx style for the xlsx headers

header.style is a openxlsx style for the xlsx body

## Usage

``` r
config
```

## Format

An object of class `list` of length 6.
