% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LinearNormalResponseCalculator.R
\docType{class}
\name{LinearNormalResponseCalculator}
\alias{LinearNormalResponseCalculator}
\title{Class for calculating linear response with normal error}
\format{\code{\link{R6Class}} object}
\usage{
LinearNormalResponseCalculator
}
\value{
Object of \code{\link{R6Class}}
}
\description{
Class for calculating linear response with normal error
}
\section{Fields}{

\describe{
\item{\code{sigma_err}}{Private.}

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
