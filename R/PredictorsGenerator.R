#' Abstract Class for generating predictors
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @field cov_mat Private. Covariance matrix used for generating predictors
#' @field num_observations Private. The number of observations
#' @field num_predictors Private. The number of predictors
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Abstract class}
#'   \item{\code{get_num_observations}}{Returns num_observations}
#'   \item{\code{get_num_predictors}}{Returns num_predictors}
#'   \item{\code{simulate_predictors}}{Abstract function}
#'   }
PredictorsGenerator <- R6::R6Class("PredictorsGenerator",
   public = list(
     initialize = function() {
       stop("Abstract Class! Cannot initialize!")
     },

     get_num_observations = function() {
       private$num_observations
     },

     get_num_predictors = function() {
       private$num_predictors
     },

     simulate_predictors = function() {
       stop("Abstract function! Cannot utilize!")
     }
   ),
   private = list(
     num_observations = NULL,
     num_predictors = NULL
   )
)
