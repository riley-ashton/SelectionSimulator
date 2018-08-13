#' Function for displaying fitted coefficient bias in a table
#' @param Simulation object
#' @importFrom knitr kable
#' @export
coeff_bias = function(Simulation, rounding_digits = 3) {
  fitted <- Simulation$get_fitted_coefficients()
  fitted[is.na(fitted)] <- 0
  fitted_mean <- apply(fitted, MARGIN = c(2,3), mean)

  actual <- Simulation$SimulationDataGenerator$
    ResponseCalculator$get_coefficients()

  knitr::kable(fitted_mean - actual,
               digits = rounding_digits, caption = "Fitted Coefficients Bias")
}

#' Function for displaying fitted coefficient variance in a table
#' @param Simulation object
#' @importFrom knitr kable
#' @export
coeff_variance = function(Simulation, rounding_digits = 3) {
  fitted <- Simulation$get_fitted_coefficients()
  fitted[is.na(fitted)] <- 0
  knitr::kable(apply(fitted, MARGIN = c(2,3), var),
               digits = rounding_digits, caption = "Fitted Coefficients Variance")
}
