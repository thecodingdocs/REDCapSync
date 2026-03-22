# REDCapSync Encapsulated Datsaset Object

[R6](https://r6.r-lib.org/reference/R6Class.html) project object for
[REDCapSync](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync-package.md)

## Value

An R6ClassGenerator which is used internally to create or load a dataset
object for the user

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for initializing the `project` object.'

## Public fields

- `data`:

  list of data where names are forms

- `metadata`:

  list of metadata

- `records`:

  data.frame of records with timestamps

- `users`:

  data.frame of users with timestamps

- `log`:

  data.frame of log

- `comments`:

  data.frame of comments

## Active bindings

- `project_name`:

  Read-only character string of project_name as assigned with
  [setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

- `dataset_name`:

  Read-only character string of dataset_name

- `n_records`:

  Read-only integer of n_records

## Methods

### Public methods

- [`REDCapSyncDataset$new()`](#method-REDCapSyncDataset-new)

- [`REDCapSyncDataset$print()`](#method-REDCapSyncDataset-print)

------------------------------------------------------------------------

### Method `new()`

The end user will not see `dataset$new()`. This is handled internally.
Users should construct objects using
[REDCapSyncProject](https://thecodingdocs.github.io/REDCapSync/reference/project.md).

#### Usage

    REDCapSyncDataset$new(project, dataset_name)

#### Arguments

- `project`:

  a list object meant to be stored internally within R6

- `dataset_name`:

  dataset name setup with `project$add_dataset`

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print some key dataset information

#### Usage

    REDCapSyncDataset$print()
