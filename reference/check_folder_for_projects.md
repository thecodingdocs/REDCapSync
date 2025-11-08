# List File Paths of REDCapSync Projects in a Folder

Searches a specified folder for files related to REDCapSync projects and
returns their file paths. Optionally validates the folder to ensure it
was previously set up using
[`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).

## Usage

``` r
check_folder_for_projects(file_path, validate = TRUE)
```

## Arguments

- file_path:

  Character. The path to the folder to search.

- validate:

  Logical. If `TRUE`, the function will only accept valid directories
  previously set up with
  [`setup_project()`](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md).
  Default is `TRUE`.

## Value

A character vector of file paths for valid REDCapSync project files in
the folder. Returns an empty character vector if no valid files are
found.

## Details

This function checks a folder (and optionally validates its setup) for
`.RData` files that correspond to REDCapSync projects. It identifies
files with the extension `.RData` and names ending in `_REDCapSync`,
filtering out any unrelated files.

## See also

[setup_project](https://thecodingdocs.github.io/REDCapSync/reference/setup-load.md)
for setting up valid directories.
