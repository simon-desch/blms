% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blmsformula-helpers.R
\name{split_by_lhs}
\alias{split_by_lhs}
\title{Split a formula into multiple formulas based on variables defined in the
left-hand side of the formula}
\usage{
split_by_lhs(form)
}
\arguments{
\item{form}{a formula, possibly containing multiple variables (typically
combined by \code{+}) on its left-hand side}
}
\value{
A \code{list()} containing \code{formula} objects created by
splitting the input formula into multiple formulas if multiple
variables are listed on it left-hand side
}
\description{
Split a formula into multiple formulas based on variables defined in the
left-hand side of the formula
}
\details{
Variables on the left-hand side of the formula are extracted using
\code{all.vars}
}
\examples{
split_by_lhs(a + b ~ c)

}
