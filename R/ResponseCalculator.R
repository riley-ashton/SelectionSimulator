#' Abstract Class for calculating response
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Abstract class}
#'   \item{\code{get_betas}}{Abstract function}
#'   \item{\code{calculate_response}}{Abstract function}
#'   \item{\code{response_is_continuous}}{Abstract function}
#'   }
ResponseCalculator <- R6::R6Class("ResponseCalculator",
 public = list(
   initialize = function() {
     stop("Abstract Class!")
   },

   get_coefficients = function() {
     stop("Abstract function!")
   },

   calculate_response = function() {
     stop("Abstract function!")
   },

   response_is_continuous = function() {
     stop("Abstract function!")
   }
 ),
 private = list()
)
