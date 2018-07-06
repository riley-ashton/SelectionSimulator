#' Class for calculating linear response with normal error
#'
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @field sigma_err Private.
#' @field betas Private.
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Abstract class}
#'   \item{\code{get_betas}}{Returns betas}
#'   \item{\code{calculate_response}}{Abstract function}
#'   \item{\code{response_is_continuous}}{Abstract function}
#'   }
LinearNormalResponseCalculator <- R6::R6Class("LinearNormalResponseCalculator",
  public = list(
    initialize = function(num_observations, num_predictors,
                          norm_rand_var_sd, covariance_matrix,
                          intercept) {
      private$num_observations <- num_observations
      private$num_predictors <- num_predictors
      private$norm_rand_var_sd <- norm_rand_var_sd
      private$covariance_matrix <- covariance_matrix
      private$intercept <- intercept
    },

    get_betas = function() {
      c(`(Intercept)` = private$intercept, private$betas)
    },

    calculate_response = function() {
      as.vector(self$X %*% self$betas) +
        self$beta_0 + rnorm(self$n, sd = self$sigma_err)
    },

    response_is_continuous = function() {
      TRUE
    }
  ),
  private = list(
    sigma_err = NULL,
    betas = NULL,
    norm_rand_var_sd = NULL,
    intercept = NULL
  )
)
