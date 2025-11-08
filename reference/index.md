# Package index

## Core Pipeline Functions (Basic)

For basic users use these core functions to maintain REDCap data
pipelines. Setup or Load your project. Update from REDCap using what is
already saved. Save the outputs to a directory.

- [`get_projects()`](https://thecodingdocs.github.io/REDCapSync/reference/get_projects.md)
  : Get your REDCap projects used by REDCapSync
- [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_test_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  : Setup or Load REDCapSync Project
- [`sync_project()`](https://thecodingdocs.github.io/REDCapSync/reference/sync_project.md)
  : Synchronize REDCap Data

## Everything by Topic (Intermediate/Advanced)

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

### Tokens

Tokens are required to use the REDCap API. Never share your tokens.

- [`view_project_token()`](https://thecodingdocs.github.io/REDCapSync/reference/view_project_token.md)
  : View the REDCap API Token Stored in the Session
- [`test_project_token()`](https://thecodingdocs.github.io/REDCapSync/reference/test_project_token.md)
  : Test REDCap API Token linked to a project Object

### Setup/Load/Save/Delete project Object

project objects are R lists meant to store all metadata and data for a
single REDCap project. They utilize the REDCap log to only fetch recent
updates. The can be “mirrored” to a local/cloud directory to maintain
file pipelines and/or used for an exploratory data analysis shiny app
via `RosyREDCap::run_RosyREDCap`.

- [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  [`load_test_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
  : Setup or Load REDCapSync Project
- [`save_project()`](https://thecodingdocs.github.io/REDCapSync/reference/save-deleteproject.md)
  : Save or Delete project file from the directory

### Sync Function

sync_project uses project object in the environment.

- [`sync_project()`](https://thecodingdocs.github.io/REDCapSync/reference/sync_project.md)
  : Synchronize REDCap Data
- [`sync_all()`](https://thecodingdocs.github.io/REDCapSync/reference/sync_all.md)
  : Synchronize REDCap Data

### Project Summaries

One of the core internal motivations for REDCapSync development was to
have REDCap-project-agnositic functions that perform key transformations
such as deidentify, derive fields, and transform (merge). However, these
functions might not be utilized by the average user.

- [`add_project_summary()`](https://thecodingdocs.github.io/REDCapSync/reference/add_project_summary.md)
  **\[experimental\]** : Add a Subset to a REDCap Database
- [`generate_project_summary()`](https://thecodingdocs.github.io/REDCapSync/reference/generate_project_summary.md)
  **\[experimental\]** : Generate a Summary from a Subset Name
- [`clear_project_summaries()`](https://thecodingdocs.github.io/REDCapSync/reference/clear_project_summaries.md)
  **\[experimental\]** : clear_project_summaries

### Data Helpers

Used for managing to-be-upload data

- [`raw_to_labelled_form()`](https://thecodingdocs.github.io/REDCapSync/reference/raw_to_labelled_form.md)
  : Raw to Labelled REDCap forms
- [`labelled_to_raw_form()`](https://thecodingdocs.github.io/REDCapSync/reference/labelled_to_raw_form.md)
  : Clean to Raw REDCap forms

### Download from, Upload to, and Delete from REDCap

Always use with caution! Package takes several steps to test and confirm
uploads but ultimately the user is responsible for data changes.
Remember REDCap always keeps a log of changes.

- [`get_REDCap_report()`](https://thecodingdocs.github.io/REDCapSync/reference/get_REDCap_report.md)
  : Get REDCap Report
- [`upload_form_to_REDCap()`](https://thecodingdocs.github.io/REDCapSync/reference/upload_form_to_REDCap.md)
  : Upload to REDCap

### Other Helpers

- [`reexports`](https://thecodingdocs.github.io/REDCapSync/reference/reexports.md)
  [`%>%`](https://thecodingdocs.github.io/REDCapSync/reference/reexports.md)
  [`sanitize_token`](https://thecodingdocs.github.io/REDCapSync/reference/reexports.md)
  : Objects exported from other packages
- [`nav_to_dir()`](https://thecodingdocs.github.io/REDCapSync/reference/nav_to_dir.md)
  : nav_to_dir
- [`link_API_token()`](https://thecodingdocs.github.io/REDCapSync/reference/Links.md)
  [`link_API_playground()`](https://thecodingdocs.github.io/REDCapSync/reference/Links.md)
  [`link_REDCap_home()`](https://thecodingdocs.github.io/REDCapSync/reference/Links.md)
  [`link_REDCap_project()`](https://thecodingdocs.github.io/REDCapSync/reference/Links.md)
  [`link_REDCap_record()`](https://thecodingdocs.github.io/REDCapSync/reference/Links.md)
  : Open Links to REDCap Pages

### Advanced/Experimental

This is diffult to explain experimental function for adding derived
fields and overwritting existing fields for advanced pipelines.

- [`add_project_field()`](https://thecodingdocs.github.io/REDCapSync/reference/add_project_field.md)
  **\[experimental\]** : Add Field Transformation to the Database
- [`clear_project_fields()`](https://thecodingdocs.github.io/REDCapSync/reference/clear_project_fields.md)
  **\[experimental\]** : clear_project_fields
