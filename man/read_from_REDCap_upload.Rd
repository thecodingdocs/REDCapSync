% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_to_and_from_dir.R
\name{read_from_REDCap_upload}
\alias{read_from_REDCap_upload}
\title{Reads project from the dropped REDCap files in dir/REDCap/upload}
\usage{
read_from_REDCap_upload(
  project,
  allow_all = TRUE,
  drop_nonredcap_vars = TRUE,
  drop_non_form_vars = TRUE,
  stop_or_warn = "warn"
)
}
\arguments{
\item{project}{A validated \code{project} object containing REDCap project data
and
settings. Generated using \link{load_project} or \link{setup_project}}

\item{allow_all}{logical TF for allowing project$data names that are not also
form names}

\item{drop_nonredcap_vars}{logical TF for dropping non-redcap variable names}

\item{drop_non_form_vars}{logical TF for dropping non-form variable names}

\item{stop_or_warn}{character string of whether to stop, warn, or do nothing
when forbidden cols are present}
}
\value{
messages for confirmation
}
\description{
Reads project from the dropped REDCap files in dir/REDCap/upload
}
