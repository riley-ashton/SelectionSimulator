#' Class for calculating response using predict of model
#'
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @section Methods:
#' \describe{
#'   \item{\code{new(object_model, irreducible_error_generator, response_is_continuous)}}{Class constructor}
#'   \item{\code{get_betas}}{Returns the coefficients of the object_model}
#'   \item{\code{calculate_response(predictors)}}{Calculates the response using prediction on the object}
#'   \item{\code{response_is_continuous()}}{Returns whether the response is continuous}
#'   }
FromModelResponseCalculator <- R6::R6Class("FromModelResponseCalculator",
  public = list(
    response_is_continuous = NULL,

    initialize = function(object_model,
                          irreducible_error_generator,
                          response_is_continuous) {
      private$object_model <- object_model
      private$irreducible_error_generator <- irreducible_error_generator
    },

    get_betas = function() {
      private$object_model$coefficients
    },

    calculate_response = function(predictors) {
      stopifnot(class(predictors) == "data.frame")
      out <- predict(private$object_model, predictors)
      sapply(out, function(x) x + private$irreducible_error_generator())
    }
  ),
  private = list(
    object_model = NULL,
    irreducible_error_generator = NULL
  )
)
