% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_project.R
\name{summarize_project}
\alias{summarize_project}
\title{Summarize REDCap Database}
\usage{
summarize_project(
  project,
  with_links = TRUE,
  deidentify = TRUE,
  clean = TRUE,
  drop_blanks = TRUE,
  include_metadata = TRUE,
  annotate_metadata = TRUE,
  include_record_summary = TRUE,
  include_users = TRUE,
  include_log = TRUE,
  separate = FALSE,
  reset = FALSE
)
}
\arguments{
\item{project}{A validated \code{project} object containing REDCap project data and
settings. Generated using \link{load_project} or \link{setup_project}}

\item{with_links}{Logical (TRUE/FALSE). If TRUE, includes links in the summary. Default is \code{TRUE}.}

\item{deidentify}{Logical (TRUE/FALSE). If TRUE, deidentifies the summary data. Default is \code{TRUE}.}

\item{clean}{Logical (TRUE/FALSE). If TRUE, cleans the summary data. Default is \code{TRUE}.}

\item{drop_blanks}{Logical (TRUE/FALSE). If TRUE, drops blank entries from the summary. Default is \code{TRUE}.}

\item{include_metadata}{Logical (TRUE/FALSE). If TRUE, includes metadata in the summary. Default is \code{TRUE}.}

\item{annotate_metadata}{Logical (TRUE/FALSE). If TRUE, annotates metadata in the summary. Default is \code{TRUE}.}

\item{include_record_summary}{Logical (TRUE/FALSE). If TRUE, includes a summary of records in the summary. Default is \code{TRUE}.}

\item{include_users}{Logical (TRUE/FALSE). If TRUE, includes user information in the summary. Default is \code{TRUE}.}

\item{include_log}{Logical (TRUE/FALSE). If TRUE, includes logs in the summary. Default is \code{TRUE}.}

\item{separate}{Logical (TRUE/FALSE). If TRUE, separates the summary into different sections. Default is \code{FALSE}.}

\item{reset}{Logical (TRUE/FALSE). If TRUE, forces the summary generation even if there are issues. Default is \code{FALSE}.}
}
\value{
List. Returns a list containing the summarized data, including records, metadata, users, logs, and any other specified data.
}
\description{
Summarizes the REDCap database (\code{project} object) by filtering and generating a summary list.
}
\details{
This function filters the REDCap database based on the provided parameters and generates a summary list. The summary can include metadata, record summaries, user information, and logs. The function also supports deidentification and cleaning of the data.
}
\seealso{
\link{setup_project} for initializing the \code{project} object.
\link{sync_project} for updating the \code{project} object.

Other db_functions: 
\code{\link{rmarkdown_project}()},
\code{\link{sync_project}()},
\code{\link{transform_project}()}
}
\concept{db_functions}
