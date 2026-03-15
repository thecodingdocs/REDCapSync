# Package index

## Setup and Load

For basic users use these core functions to maintain REDCap data
pipelines. Setup or Load your project. Update from REDCap using what is
already saved. Save the outputs to a directory.

- [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  : Setup or Load REDCapSync Project

## Project Object

Once setup, the project object can be used to run sync and define custom
transformations and subset to be passed to spreadsheets.

- [`REDCapSyncProject`](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSyncProject.md)
  : REDCapSync Encapsulated Project Object
- [`config`](https://thecodingdocs.github.io/REDCapSync/reference/config.md)
  : Package configuration accessors

## Multiple Projects

Once setup, the project object can be used to run sync and define custom
transformations and subset to be passed to spreadsheets

- [`sync()`](https://thecodingdocs.github.io/REDCapSync/reference/sync.md)
  : Synchronize REDCap Data

## Project Cache

REDCapSync cahces user-level project metadata (name, directory, links,
last update and other details). Tokens and data are NOT stored here.

- [`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)
  : Get your REDCap projects used by REDCapSync
- [`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md)
  : Clear your cached projects
