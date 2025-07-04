% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doc-helper.R
\name{parameter_names_parsed_string}
\alias{parameter_names_parsed_string}
\title{Create string to display parameter names using Latex equation}
\usage{
parameter_names_parsed_string(model_spec)
}
\arguments{
\item{model_spec}{A \code{model_spec} object.}
}
\value{
A string with formatting for parameter names.
}
\description{
Create string to display parameter names using Latex equation
}
\examples{
parameter_names_parsed_string(get_model_spec('ql_a_it'))
}
