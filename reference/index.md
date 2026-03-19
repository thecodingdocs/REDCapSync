# Package index

## Setup/Load and Use Single Project

Setup or Load REDCap your project and update using what is already
saved. Define custom transformations and subsets to be passed to
refreshing spreadsheets.

- [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  : Setup or Load REDCapSync Project
- [`project`](https://thecodingdocs.github.io/REDCapSync/reference/project.md)
  [`REDCapSyncProject`](https://thecodingdocs.github.io/REDCapSync/reference/project.md)
  : REDCapSync Encapsulated Project Object

## Multiple Projects

The projects object can act a single entry point for all of your
projects! Sync is a hands-free function that will sync all projects
based on previous settings.

- [`projects`](https://thecodingdocs.github.io/REDCapSync/reference/projects.md)
  : REDCap projects used by REDCapSync
- [`sync()`](https://thecodingdocs.github.io/REDCapSync/reference/sync.md)
  : Synchronize REDCap Data

## Configuration

Change user-level settings and clear cache

- [`config`](https://thecodingdocs.github.io/REDCapSync/reference/config.md)
  : Configuration
- [`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md)
  : Clear your cached projects
