% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_project.R
\name{transform_project}
\alias{transform_project}
\title{transform_project}
\usage{
transform_project(project, reset = FALSE)
}
\arguments{
\item{project}{A validated \code{project} object containing REDCap project data
and
settings. Generated using \link{load_project} or \link{setup_project}}

\item{reset}{Logical that forces transformation if TRUE. Default is \code{FALSE}.}
}
\value{
The transformed \code{project} object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
Transforms the REDCap database (\code{project} object) by applying the necessary
field transformations. This function modifies the structure of the data and
records according to the transformation rules specified.
}
\details{
This function checks if the database has already been transformed and applies
the transformation if not. It stores the original column names before
transforming the data. The transformation process can include modifying field
values and renaming columns based on predefined transformation rules.
}
\seealso{
\code{\link[REDCapSync]{save_project}} for saving the transformed database
object.

Other db_functions: 
\code{\link{rmarkdown_project}()},
\code{\link{summarize_project}()},
\code{\link{sync_project}()}
}
\concept{db_functions}
