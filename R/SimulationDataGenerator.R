#' Class for simulating data given
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @field PredictorsGenerator object for generating predictors
#' @field ResponseCalculator object for computing response
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor}
#'   \item{\code{simulate_data}}{Returns a new simulated data set}
#'   \item{\code{get_response_name}}{Returns "response"}
#'   }

SimulationDataGenerator <- R6::R6Class("SimulationDataGenerator",
  public = list(
    PredictorsGenerator = NULL,
    ResponseCalculator = NULL,

    initialize = function(PredictorsGenerator, ResponseCalculator) {
      stopifnot("PredictorsGenerator" %in% class(PredictorsGenerator))
      stopifnot("ResponseCalculator" %in% class(ResponseCalculator))

      self$PredictorsGenerator <- PredictorsGenerator
      self$ResponseCalculator <- ResponseCalculator
    },

    simulate_data = function() {
      predictors <- self$PredictorsGenerator$simulate_predictors()
      response <- self$ResponseCalculator$calculate_response(predictors)
      cbind(predictors, response)
    },

    get_response_name = function() {
      "response"
    }
  ),
  private = list()
)
