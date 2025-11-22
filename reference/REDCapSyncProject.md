# Setup or Load REDCapSync Project

Project class (REDCapSync) Main class for managing REDCap data,
metadata, and sync operations. Users should construct objects using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md),
not using `REDCapSyncProject$new()` directly.

## Value

A `REDCapSyncProject` R6 object.

## Methods

### Public methods

- [`REDCapSyncProject$new()`](#method-REDCapSyncProject-new)

- [`REDCapSyncProject$info()`](#method-REDCapSyncProject-info)

- [`REDCapSyncProject$add_summary()`](#method-REDCapSyncProject-add_summary)

- [`REDCapSyncProject$generate_summary()`](#method-REDCapSyncProject-generate_summary)

- [`REDCapSyncProject$add_field()`](#method-REDCapSyncProject-add_field)

- [`REDCapSyncProject$sync()`](#method-REDCapSyncProject-sync)

- [`REDCapSyncProject$save()`](#method-REDCapSyncProject-save)

- [`REDCapSyncProject$use()`](#method-REDCapSyncProject-use)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this REDCapSyncProject
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    REDCapSyncProject$new(short_name)

#### Arguments

- `short_name`:

  Character project identifier.

------------------------------------------------------------------------

### Method `info()`

Print project metadata

#### Usage

    REDCapSyncProject$info()

------------------------------------------------------------------------

### Method `add_summary()`

Add a new summary entry

#### Usage

    REDCapSyncProject$add_summary()

------------------------------------------------------------------------

### Method `generate_summary()`

Add a new summary entry

#### Usage

    REDCapSyncProject$generate_summary(short_name)

#### Arguments

- `short_name`:

  Character project identifier.

------------------------------------------------------------------------

### Method `add_field()`

Add a new summary entry

#### Usage

    REDCapSyncProject$add_field()

------------------------------------------------------------------------

### Method `sync()`

Add a new summary entry

#### Usage

    REDCapSyncProject$sync()

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Add a new summary entry

#### Usage

    REDCapSyncProject$save()

------------------------------------------------------------------------

### Method [`use()`](https://rdrr.io/r/base/use.html)

returns internal list

#### Usage

    REDCapSyncProject$use()
