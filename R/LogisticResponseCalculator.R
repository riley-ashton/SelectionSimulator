#' Class for calculating logistic (binary) response
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @field coefficients Private.
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor}
#'   \item{\code{get_coefficients}}{Returns coefficients}
#'   \item{\code{calculate_response}}{Calculates the response}
#'   \item{\code{response_is_continuous}}{Returns TRUE}
#'   }
LogisticResponseCalculator <- R6::R6Class("LogisticResponseCalculator",
  inherit = ResponseCalculator,
  public = list(
    initialize = function(coefficients, intercept) {
      private$intercept <- intercept
      private$coefficients <- coefficients
    },

    get_coefficients = function() {
      c(`(Intercept)` = private$intercept, private$coefficients)
    },

    calculate_response = function(predictors) {
      predictors <- as.matrix(predictors)

      XB <- as.vector(predictors %*% private$coefficients) + private$intercept
      p <- exp(XB) / (1 + exp(XB))
      sapply(p, function(p) rbinom(1,1, p))
    },

    response_is_continuous = function() {
      FALSE
    }
  ),
  private = list(
    coefficients = NULL,
    intercept = NULL
  )
)
