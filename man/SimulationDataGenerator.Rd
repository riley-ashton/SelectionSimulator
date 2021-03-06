% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimulationDataGenerator.R
\docType{class}
\name{SimulationDataGenerator}
\alias{SimulationDataGenerator}
\title{Class for simulating data given}
\format{\code{\link{R6Class}} object}
\usage{
SimulationDataGenerator
}
\value{
Object of \code{\link{R6Class}}
}
\description{
Class for simulating data given
}
\section{Fields}{

\describe{
\item{\code{PredictorsGenerator}}{object for generating predictors}

\item{\code{ResponseCalculator}}{object for computing response}
}}

\section{Methods}{

\describe{
  \item{\code{new}}{Constructor}
  \item{\code{simulate_data}}{Returns a new simulated data set}
  \item{\code{get_response_name}}{Returns "response"}
  }
}

\keyword{datasets}
