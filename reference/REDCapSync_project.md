# Setup or Load REDCapSync Project

[R6](https://r6.r-lib.org/reference/R6Class.html) project object for
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)
Main class for managing REDCap data, metadata, and sync operations.
Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

## Value

An R6ClassGenerator

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.'

## Methods

### Public methods

- [`REDCapSync_project$new()`](#method-REDCapSync_project-new)

- [`REDCapSync_project$info()`](#method-REDCapSync_project-info)

- [`REDCapSync_project$add_summary()`](#method-REDCapSync_project-add_summary)

- [`REDCapSync_project$generate_summary()`](#method-REDCapSync_project-generate_summary)

- [`REDCapSync_project$add_field()`](#method-REDCapSync_project-add_field)

- [`REDCapSync_project$sync()`](#method-REDCapSync_project-sync)

- [`REDCapSync_project$save()`](#method-REDCapSync_project-save)

- [`REDCapSync_project$show_metadata()`](#method-REDCapSync_project-show_metadata)

- [`REDCapSync_project$show_data()`](#method-REDCapSync_project-show_data)

- [`REDCapSync_project$use()`](#method-REDCapSync_project-use)

------------------------------------------------------------------------

### Method `new()`

The end user will not see `project$new()`. This is handled internally.
Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
The remain methods will be accessible to any user.

#### Usage

    REDCapSync_project$new(project)

#### Arguments

- `project`:

  a list object meant to be stored internally within R6

------------------------------------------------------------------------

### Method `info()`

Print project metadata

#### Usage

    REDCapSync_project$info()

------------------------------------------------------------------------

### Method `add_summary()`

Add a new summary entry

#### Usage

    REDCapSync_project$add_summary()

------------------------------------------------------------------------

### Method `generate_summary()`

Add a new summary entry

#### Usage

    REDCapSync_project$generate_summary(summary_name)

#### Arguments

- `summary_name`:

  Character of summary_name.

------------------------------------------------------------------------

### Method `add_field()`

Add a new summary entry

#### Usage

    REDCapSync_project$add_field()

------------------------------------------------------------------------

### Method [`sync()`](https://thecodingdocs.github.io/REDCapSync/reference/sync.md)

Updates the REDCap database (`project` object) by fetching the latest
data from the REDCap server.

#### Usage

    REDCapSync_project$sync(
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

    REDCapSync_project$save()

------------------------------------------------------------------------

### Method `show_metadata()`

Returns list of data or the specified form.

#### Usage

    REDCapSync_project$show_metadata(type = "fields")

#### Arguments

- `type`:

  string of either "fields","forms", or "choices"

------------------------------------------------------------------------

### Method `show_data()`

Returns list of data or the specified form.

#### Usage

    REDCapSync_project$show_data(form)

#### Arguments

- `form`:

  string of raw form name such as "survey_one"

------------------------------------------------------------------------

### Method [`use()`](https://rdrr.io/r/base/use.html)

returns internal list

#### Usage

    REDCapSync_project$use()
