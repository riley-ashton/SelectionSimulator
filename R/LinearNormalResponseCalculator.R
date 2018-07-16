#' Class for calculating linear response with normal error
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @field sigma_err Private.
#' @field coefficients Private.
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor}
#'   \item{\code{get_coefficients}}{Returns coefficients}
#'   \item{\code{calculate_response}}{Calculates the response}
#'   \item{\code{response_is_continuous}}{Returns TRUE}
#'   }
LinearNormalResponseCalculator <- R6::R6Class("LinearNormalResponseCalculator",
  inherit = ResponseCalculator,
  public = list(
    initialize = function(norm_rand_var_sd, coefficients, intercept) {
      private$norm_rand_var_sd <- norm_rand_var_sd
      private$intercept <- intercept
      private$coefficients <- coefficients
    },

    get_coefficients = function() {
      c(`(Intercept)` = private$intercept, private$coefficients)
    },

    calculate_response = function(predictors) {
      predictors <- as.matrix(predictors)
      out <- as.vector(predictors %*% private$coefficients) +
        private$intercept +
        rnorm(nrow(predictors), sd = private$norm_rand_var_sd)
      as.vector(out)
    },

    response_is_continuous = function() {
      TRUE
    }
  ),
  private = list(
    coefficients = NULL,
    norm_rand_var_sd = NULL,
    intercept = NULL
  )
)
