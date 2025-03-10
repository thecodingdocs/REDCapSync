% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_project.R
\name{add_project_summary}
\alias{add_project_summary}
\title{Add a Subset to a REDCap Database}
\usage{
add_project_summary(
  project,
  subset_name,
  transform = TRUE,
  filter_field = NULL,
  filter_choices = NULL,
  filter_list = NULL,
  filter_strict = TRUE,
  field_names = NULL,
  form_names = NULL,
  no_duplicate_cols = FALSE,
  deidentify = TRUE,
  drop_free_text = FALSE,
  clean = TRUE,
  drop_blanks = TRUE,
  include_metadata = TRUE,
  annotate_metadata = TRUE,
  include_record_summary = TRUE,
  include_users = TRUE,
  include_log = TRUE,
  with_links = TRUE,
  separate = FALSE,
  use_csv,
  dir_other = file.path(project$dir_path, "output"),
  file_name = paste0(project$short_name, "_", subset_name),
  reset = FALSE
)
}
\arguments{
\item{project}{A validated \code{project} object containing REDCap project data
and
settings. Generated using \link{load_project} or \link{setup_project}}

\item{subset_name}{Character. The name of the subset to create.}

\item{transform}{Logical. Whether to transform the data in the subset.
Default
is \code{TRUE}.}

\item{filter_field}{Character. The name of the field in the database to
filter on.}

\item{filter_choices}{Vector. The values of \code{filter_field} used to define the
subset.}

\item{filter_list}{Vector. The values of \code{filter_field} used to define the
subset.}

\item{filter_strict}{Logical. If \code{TRUE}, all forms will be filtered by
criteria. If \code{FALSE}, will convert original filter to id column and filter
all other forms by that record. Default is \code{TRUE}.}

\item{field_names}{Character vector. Names of specific fields to include in
the subset. Default is \code{NULL}, which includes all fields.}

\item{form_names}{Character vector. Names of forms to include in the subset.
Default is \code{NULL}, which includes all forms.}

\item{no_duplicate_cols}{A logical flag (\code{TRUE} or \code{FALSE}). If \code{TRUE}, the
function will avoid including duplicate columns in the output. Defaults to
\code{FALSE}.}

\item{deidentify}{Logical. Whether to deidentify the data in the subset.
Default is \code{TRUE}.}

\item{drop_free_text}{Logical. If \code{TRUE}, columns containing free text
will also be excluded from the \code{project}. Default is \code{FALSE}.}

\item{clean}{Logical. If \code{TRUE}, the data will be cleaned before summarizing.
Default is \code{TRUE}.}

\item{drop_blanks}{Logical. If \code{TRUE}, records with blank fields will be
dropped. Default is \code{TRUE}.}

\item{include_metadata}{Logical. If \code{TRUE}, metadata will be included in the
summary. Default is \code{TRUE}.}

\item{annotate_metadata}{Logical. If \code{TRUE}, metadata will be annotated in
the summary. Default is \code{TRUE}.}

\item{include_record_summary}{Logical. If \code{TRUE}, a record summary will be
included in the generated summary. Default is \code{TRUE}.}

\item{include_users}{Logical. If \code{TRUE}, user-related information will be
included in the summary. Default is \code{TRUE}.}

\item{include_log}{Logical. If \code{TRUE}, the log of changes will be included in
the summary. Default is \code{TRUE}.}

\item{with_links}{Optional logical (TRUE/FALSE) for including links in Excel
sheets. Default is \code{FALSE}.}

\item{separate}{Optional logical (TRUE/FALSE) separating each form into
separate files as opposed to multi-tab Excel. Default is \code{FALSE}.}

\item{use_csv}{Logical (TRUE/FALSE). If TRUE, uses CSV files for data
storage. Default is \code{FALSE}.}

\item{dir_other}{Character. The directory where the subset file will be
saved. Default is the \code{output} folder within the database directory.}

\item{file_name}{Character. The base name of the file where the subset will
be saved. Default is \verb{<project$short_name>_<subset_name>}.}

\item{reset}{Logical. If \code{TRUE}, overwrite existing subset files with the
same name. Default is \code{FALSE}.}
}
\value{
A modified \code{project} object that includes the newly created subset.
The subset is also saved as a file in the specified directory.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
Creates a subset of the main REDCap database (\code{project}) based on specific
filter criteria and saves it to a specified directory. The subset can be
further customized with additional forms, fields, and deidentification
options.
}
\details{
This function filters the main REDCap database using the specified
\code{filter_field}
and \code{filter_choices}, then creates a new subset with optional
deidentification. It can be customized to include only specific forms or
fields. The resulting subset is saved to a file for future use.
}
\seealso{
\code{\link{save_project}} for saving the main database or subsets.
}
