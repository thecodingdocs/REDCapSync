% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_project.R
\name{add_project_transformation}
\alias{add_project_transformation}
\alias{default_project_transformation}
\title{Add Default Forms Transformation to the Database}
\usage{
add_project_transformation(project, forms_transformation)

default_project_transformation(project)
}
\arguments{
\item{project}{A validated \code{project} object containing REDCap project data
and
settings. Generated using \link{load_project} or \link{setup_project}}

\item{forms_transformation}{a data.frame that matches instruments. See
\code{default_project_transformation} for an example.}
}
\value{
The updated \code{project} object with default transformations applied to the
specified forms.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
Applies default transformations to specific forms within the REDCap database
(\code{project}).
This function modifies the \code{project} object to include default
transformations, which may
involve adjustments, calculations, or reformatting of data in predefined
forms.
}
\details{
This function is designed to streamline and standardize data processing by
applying default transformations to the database forms. The transformations
are predefined within the function and ensure consistency across datasets.
}
\seealso{
\code{\link{save_project}} for saving the database or summaries.
}
