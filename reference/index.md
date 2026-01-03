# Package index

## Core Pipeline Functions (Basic)

For basic users use these core functions to maintain REDCap data
pipelines. Setup or Load your project. Update from REDCap using what is
already saved. Save the outputs to a directory.

- [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_test_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  : Setup or Load REDCapSync Project
- [`sync()`](https://thecodingdocs.github.io/REDCapSync/reference/sync.md)
  : REDCapSync Encapsulated Project Object

## Other (Intermediate/Advanced)

Expands on functionality and breaksdown core feautures into more
customizable parts

### Project Cache

REDCapSync cahces user-level project metadata (name, directory, links,
last update and other details). Tokens and data are NOT stored here.

- [`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)
  : Get your REDCap projects used by REDCapSync
- [`cache_clear()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_clear.md)
  : Clear your cached projects
- [`cache_path()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_path.md)
  : Get your Get Cache Path
- [`cache_remove_project()`](https://thecodingdocs.github.io/REDCapSync/reference/cache_remove_project.md)
  : Remove project from cache
- [`sync()`](https://thecodingdocs.github.io/REDCapSync/reference/sync.md)
  : REDCapSync Encapsulated Project Object
