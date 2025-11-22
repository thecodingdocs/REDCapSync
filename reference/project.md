# Setup or Load REDCapSync Project

Project class (REDCapSync) Main class for managing REDCap data,
metadata, and sync operations. Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)

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

Creates a new instance of this project
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

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
