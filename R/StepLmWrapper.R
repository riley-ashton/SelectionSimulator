#' Wrapper class for basic Stepwise regression
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor}
#'   \item{\code{run}}{Runs the stepwise regression and saves the fitted model and the order the covariates were included}
#'   \item{\code{get_fitted_model}}{Returns the glm or lm object (must call "run()" first)}
#'   \item{\code{get_inclusion_order}}{Returns the order covariates were included in (list<vector<string>>) (must call "run()" first)}
#'   }
StepLmWrapper <- R6::R6Class("StepLmWrapper",
  public = list(
    initialize = function(data, response_variable,
                          starting_formula, stepwise_direction,
                          k, glm_family = gaussian) {
      private$data <- data
      private$response_variable <- response_variable
      private$starting_formula <- starting_formula
      private$stepwise_direction <- stepwise_direction
      private$k <- k
      private$glm_family <- glm_family
    },

    get_fitted_model = function() {
      private$fitted_model
    },

    get_inclusion_order = function() {
      private$inclusion_order
    },

    run = function() {
      full <- formula(lm(as.formula(paste0(private$response_variable, "~ .")), data = private$data))
      private$fitted_model <- step(glm(as.formula(paste0(private$response_variable, "~ 1")),
                                       family = private$glm_family,
                                       data = private$data),
                                   scope = full, direction = private$stepwise_direction,
                                   k = private$k, trace = 0)
      private$inclusion_order <- setdiff(names(private$fitted_model$coefficients), "(Intercept)")
      }
  ),
  private = list(
    data = NULL,
    response_variable = NULL,
    starting_formula = NULL,
    k = NULL,
    fitted_model = NULL,
    inclusion_order = NULL,
    stepwise_direction = NULL,
    glm_family = NULL
  )
)
