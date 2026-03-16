# Configuration

Internal configuration helpers used to retrieve package configuration
values from options or environment variables.

Configuration is resolved in the following order:

1.  `getOption("redcapsync.config.option.name")`

2.  `Sys.getenv("REDCAPSYNC_CONFIG_OPTION_NAME")` \# skipped for
    functions

3.  Default if unable to find and validate from above.

## Usage

``` r
config
```

## Format

An object of class `list` of length 9.

## Details

### allow.test.names

Logical for
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
allowing `project_name` starting with TEST\_. Default is `FALSE`.

    # check current value package is using...
    config$allow.test.names()
    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.allow.test.names") # get
    options(redcapsync.config.allow.test.names = FALSE) # set

    # set with ennvar (which will be prioritized when options not defined)
    Sys.getenv("REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES")  # get
    Sys.setenv(REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES = FALSE) # or set in .Renviron

### show.api.messages

Logical for showing display API messages from REDCapR and redcapAPI.
Default is `FALSE`.

    # check current value package is using...
    config$show.api.messages()
    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.show.api.messages") # get
    options(redcapsync.config.show.api.messages = FALSE) # set

    # set with ennvar (which will be prioritized when options not defined)
    Sys.getenv("REDCAPSYNC_CONFIG_SHOW_API_MESSAGES") # get
    Sys.setenv(REDCAPSYNC_CONFIG_SHOW_API_MESSAGES = FALSE) # or set in .Renviron

### verbose

Logical for showing display API messages from REDCapR and redcapAPI.
Default is `FALSE`.

    # check current value package is using...
    config$verbose()
    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.verbose") # get
    options(redcapsync.config.verbose = FALSE) # set

    # set with ennvar (which will be prioritized when options not defined)
    Sys.getenv("REDCAPSYNC_CONFIG_VERBOSE") # get
    Sys.setenv(REDCAPSYNC_CONFIG_VERBOSE = FALSE) # or set in .Renviron

### offline

Logical for offline, which if TRUE will block any API calls. Default is
`FALSE`.

    # check current value package is using...
    config$offline()
    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.offline") # get
    options(redcapsync.config.offline = FALSE) # set

    # set with ennvar (which will be prioritized when options not defined)
    Sys.getenv("REDCAPSYNC_CONFIG_OFFLINE") # get
    Sys.setenv(REDCAPSYNC_CONFIG_OFFLINE = FALSE) # or set in .Renviron

### cache.dir

Character file path overriding the default cache directory. Default
follow system standards via rappdir, hoardr, or R_USER_CACHE_DIR

    # check current value package is using...
    config$cache.dir()
    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.cache.dir") # get
    options(redcapsync.config.cache.dir = "file/path/to/keep/cache") # set

    # set with ennvar (which will be prioritized when options not defined)
    Sys.getenv("REDCAPSYNC_CONFIG_CACHE_DIR") # get
    Sys.setenv(REDCAPSYNC_CONFIG_CACHE_DIR = "file/path/to/keep/cache") # set

### keyring

Character keyring name (parameter from
[`keyring`](https://keyring.r-lib.org/reference/keyring-package.html)
package). Default is NULL, which is at the system level. For locking use
a keyring like "REDCapSync"

    # check current value package is using...
    config$keyring()
    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.keyring") # get
    options(redcapsync.config.keyring = "REDCapSync") # set

    # set with ennvar (which will be prioritized when options not defined)
    Sys.getenv("REDCAPSYNC_CONFIG_KEYRING") # get
    Sys.setenv(REDCAPSYNC_CONFIG_KEYRING = "REDCapSync") # set in session

### keyring.service

Character keyring name (parameter from
[`keyring`](https://keyring.r-lib.org/reference/keyring-package.html)
package). Default is NULL, which is at the system level. For locking use
a keyring like "REDCapSync"

    # check current value package is using...
    config$keyring.service()
    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.keyring.service") # get
    options(redcapsync.config.keyring.service = "REDCapSync") # set

    # set with ennvar (which will be prioritized when options not defined)
    Sys.getenv("REDCAPSYNC_CONFIG_KEYRING_SERVICE") # get
    Sys.setenv(REDCAPSYNC_CONFIG_KEYRING_SERVICE = "REDCapSync") # set in session

### openxlsx.header.style

Excel sheet header using
[`openxlsx::createStyle()`](https://rdrr.io/pkg/openxlsx/man/createStyle.html)

    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.openxlsx.header.style") # get
    new_style <- openxlsx::createStyle(fontSize = 12L)
    options(redcapsync.config.openxlsx.header.style = new_style)
    # does not look up envvar due to it's function type

### openxlsx.body.style

Excel sheet body using
[`openxlsx::createStyle()`](https://rdrr.io/pkg/openxlsx/man/createStyle.html)

    # set with options (which will be prioritized over envvar)
    getOption("redcapsync.config.openxlsx.body.style") # get
    new_style <- openxlsx::createStyle(fontSize = 12L)
    options(redcapsync.config.openxlsx.body.style = new_style)
    # does not look up envvar due to it's function type

## Option Names (searched first)

    option_list <- list(
      redcapsync.config.allow.test.names = NULL,
      redcapsync.config.show.api.messages = NULL,
      redcapsync.config.verbose = NULL,
      redcapsync.config.offline = NULL,
      redcapsync.config.cache.dir = NULL,
      redcapsync.config.keyring = NULL,
      redcapsync.config.keyring.service = NULL,
      redcapsync.config.openxlsx.header.style = NULL,
      redcapsync.config.openxlsx.body.style = NULL
    )

## Environment Variable Names (searched second)

    envvar_list <- list(
      REDCAPSYNC_CONFIG_ALLOW_TEST_NAMES = NA,
      REDCAPSYNC_CONFIG_SHOW_API_MESSAGES = NA,
      REDCAPSYNC_CONFIG_VERBOSE = NA,
      REDCAPSYNC_CONFIG_OFFLINE = NA,
      R_USER_CACHE_DIR = NA, # affects your entire cache for any package
      REDCAPSYNC_CONFIG_CACHE_DIR = NA, # affects only REDCapSync Cache
      REDCAPSYNC_CONFIG_KEYRING = NA,
      REDCAPSYNC_CONFIG_KEYRING_SERVICE = NA
    )

## See also

See
[`vignette("Tokens", package = "REDCapSync")`](https://thecodingdocs.github.io/REDCapSync/articles/Tokens.md)
