# Setup or Load REDCapSync Project

[R6](https://r6.r-lib.org/reference/R6Class.html) project object for
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)
Main class for managing REDCap data, metadata, and sync operations.
Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

## Value

A `project` R6 object.

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

Add a new summary entry

#### Usage

    project$sync()

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
