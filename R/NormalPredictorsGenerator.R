#' Class for generating correlated continuous normal predictors
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field covariance_matrix Private. Covariance matrix used for generating predictors
#' @field num_observations Private. The number of observations
#' @field num_predictors Private. The number of predictors
#' @field predictor_names Private. The names of the predictors
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor(num_observations : int,
#'    num_predictors : int, covariance_matrix : matrix<double>,
#'    predictor_names : vector<string>}
#'   \item{\code{get_num_observations}}{Returns num_observations}
#'   \item{\code{get_num_predictors}}{Returns num_predictors}
#'   \item{\code{simulate_predictors}}{Returns matrix of predictors}
#'   }
NormalPredictorsGenerator <- R6::R6Class("NormalPredictorsGenerator",
  inherit = PredictorsGenerator,
  public = list(
    initialize = function(num_observations, num_predictors,
                         norm_rand_var_sd, covariance_matrix,
                         predictor_names) {
      private$num_observations <- num_observations
      private$num_predictors <- num_predictors
      private$norm_rand_var_sd <- norm_rand_var_sd
      private$covariance_matrix <- covariance_matrix
      private$predictor_names <- predictor_names
    },

    simulate_predictors = function() {
      Z <- matrix(rnorm(private$num_predictors * private$num_observations),
                  nrow = private$num_observations)
      C <- chol(private$covariance_matrix)
      out <- as.data.frame(Z %*% C)
      colnames(out) <- private$predictor_names
      out
    }
  ),
  private = list(
    norm_rand_var_sd = NULL,
    covariance_matrix = NULL,
    predictor_names = NULL
  )
)
