# Package index

## Setup/Load and Project

Setup or Load REDCap your project and update using what is already
saved. Define custom datasets to be passed to refreshing spreadsheets.

- [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  : Setup or Load REDCapSync Project
- [`project`](https://thecodingdocs.github.io/REDCapSync/reference/project.md)
  [`REDCapSyncProject`](https://thecodingdocs.github.io/REDCapSync/reference/project.md)
  : REDCapSync Project Object
- [`dataset`](https://thecodingdocs.github.io/REDCapSync/reference/dataset.md)
  [`REDCapSyncDataset`](https://thecodingdocs.github.io/REDCapSync/reference/dataset.md)
  **\[experimental\]** : Standardized Dataset from REDCap Project

## Multiple Projects

The projects object can act a single entry point for all of your
projects and sync is a hands-free function that will sync all projects
based on previous activity.

- [`projects`](https://thecodingdocs.github.io/REDCapSync/reference/projects.md)
  : Manage REDCapSync projects
- [`sync()`](https://thecodingdocs.github.io/REDCapSync/reference/sync.md)
  : Synchronize REDCap Data

## Configuration

Change user-level settings and clear cache

- [`config`](https://thecodingdocs.github.io/REDCapSync/reference/config.md)
  **\[experimental\]** : Configuration
- [`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md)
  : Clear your cached projects
