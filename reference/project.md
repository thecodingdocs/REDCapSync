# Setup or Load REDCapSync Project

[R6](https://r6.r-lib.org/reference/R6Class.html) project object for
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)
Main class for managing REDCap data, metadata, and sync operations.
Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

## Value

A `project` R6 object.

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.'

## Methods

### Public methods

- [`project$new()`](#method-project-new)

- [`project$info()`](#method-project-info)

- [`project$add_summary()`](#method-project-add_summary)

- [`project$generate_summary()`](#method-project-generate_summary)

- [`project$add_field()`](#method-project-add_field)

- [`project$sync()`](#method-project-sync)

- [`project$save()`](#method-project-save)

- [`project$use()`](#method-project-use)

------------------------------------------------------------------------

### Method `new()`

The end user will not see `project$new()`. This is handled internally.
Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
The remain methods will be accessible to any user.

#### Usage

    project$new(short_name)

#### Arguments

- `short_name`:

  Character project identifier.

------------------------------------------------------------------------

### Method `info()`

Print project metadata

#### Usage

    project$info()

------------------------------------------------------------------------

### Method `add_summary()`

Add a new summary entry

#### Usage

    project$add_summary()

------------------------------------------------------------------------

### Method `generate_summary()`

Add a new summary entry

#### Usage

    project$generate_summary(short_name)

#### Arguments

- `short_name`:

  Character project identifier.

------------------------------------------------------------------------

### Method `add_field()`

Add a new summary entry

#### Usage

    project$add_field()

------------------------------------------------------------------------

### Method `sync()`

Updates the REDCap database (`project` object) by fetching the latest
data from the REDCap server.

#### Usage

    project$sync(
      summarize = TRUE,
      save_to_dir = TRUE,
      hard_check = FALSE,
      hard_reset = FALSE
    )

#### Arguments

- `summarize`:

  Logical (TRUE/FALSE). If TRUE, summarizes data to directory.

- `save_to_dir`:

  Logical (TRUE/FALSE). If TRUE, saves the updated data to the
  directory. Default is `TRUE`.

- `hard_check`:

  Will check REDCap even if not due (see `sync_frequency` parameter from
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md))

- `hard_reset`:

  Logical that forces a fresh update if TRUE. Default is `FALSE`.

#### Details

This function updates the REDCap database by fetching the latest data
from the REDCap server. It supports various options such as forcing a
fresh update, checking logs for a specified number of days, and
retrieving files from REDCap. The function can also handle metadata-only
updates and batch processing.

#### Returns

Messages for confirmation.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Add a new summary entry

#### Usage

    project$save()

------------------------------------------------------------------------

### Method [`use()`](https://rdrr.io/r/base/use.html)

returns internal list

#### Usage

    project$use()
