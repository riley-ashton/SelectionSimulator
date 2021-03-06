% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LogisticResponseCalculator.R
\docType{class}
\name{LogisticResponseCalculator}
\alias{LogisticResponseCalculator}
\title{Class for calculating logistic (binary) response}
\format{\code{\link{R6Class}} object}
\usage{
LogisticResponseCalculator
}
\value{
Object of \code{\link{R6Class}}
}
\description{
Class for calculating logistic (binary) response
}
\section{Fields}{

\describe{
\item{\code{coefficients}}{Private.}
}}

\section{Methods}{

\describe{
  \item{\code{new}}{Constructor}
  \item{\code{get_coefficients}}{Returns coefficients}
  \item{\code{calculate_response}}{Calculates the response}
  \item{\code{response_is_continuous}}{Returns TRUE}
  }
}

\keyword{datasets}
