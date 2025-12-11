# Package index

## Core Pipeline Functions (Basic)

For basic users use these core functions to maintain REDCap data
pipelines. Setup or Load your project. Update from REDCap using what is
already saved. Save the outputs to a directory.

- [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_test_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  : Setup or Load REDCapSync Project
- [`REDCapSync_project`](https://thecodingdocs.github.io/REDCapSync/reference/REDCapSync_project.md)
  : Setup or Load REDCapSync Project

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
  : Synchronize REDCap Data

### Other Helpers

- [`raw_to_labelled_form()`](https://thecodingdocs.github.io/REDCapSync/reference/raw_to_labelled_form.md)
  : Raw to Labelled REDCap forms
- [`labelled_to_raw_form()`](https://thecodingdocs.github.io/REDCapSync/reference/labelled_to_raw_form.md)
  : Clean to Raw REDCap forms
