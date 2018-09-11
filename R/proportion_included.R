#' Function for displaying the percent of the time each covariate is included
#' @param Simulation Simulation object
#' @import knitr
#' @export
proportion_included <- function(Simulation) {
  coefficients <- Simulation$SimulationDataGenerator$ResponseCalculator$get_coefficients()
  predictor_names <- setdiff(names(coefficients), c("(Intercept)"))
  fitted <- Simulation$get_fitted_coefficients()
  algo_names <-colnames(Simulation$get_test_sse())

  out <- sapply(algo_names, function(i) {
    sapply(predictor_names, function(y){
      length(Filter(Negate(is.na), fitted[,y,i])) / length(fitted[,y,i])
    })
  })
  knitr::kable(out, align='c', caption = "% Simulations Including Covariates")
}

#' Function for displaying the percent of the time the correct model is selected
#' @param Simulation Simulation object
#' @import knitr
#' @export
only_correct_predictors_included <- function(Simulation) {
  coefficients <- Simulation$SimulationDataGenerator$ResponseCalculator$get_coefficients()
  predictor_names <- setdiff(names(coefficients), c("(Intercept)"))
  correct_predictors <- setdiff(names(coefficients[coefficients != 0]),
                                c("(Intercept)"))
  fitted <- Simulation$get_fitted_coefficients()[,predictor_names,] #w/o Intercept
  algo_names <-colnames(Simulation$get_test_sse())

  out <- sapply(algo_names, function(i) {
    sum(apply(fitted[,,i], MARGIN = 1, function(x) {
      all(sapply(correct_predictors, function(y) ! is.na(x[[y]]))) &&
        ((sum(Negate(is.na)(x))) == length(correct_predictors))
      }) / nrow(fitted)
      )
  })
  knitr::kable(out, align='c',
               caption = "% Simulations that Selected Correct Model")
}

#' Function for displaying the percent of the time the correct model or the
#'  correct model with additional covariates is selected
#' @param Simulation Simulation object
#' @import knitr
#' @export
at_least_correct_predictors_included <- function(Simulation) {
  coefficients <- Simulation$SimulationDataGenerator$ResponseCalculator$get_coefficients()
  predictor_names <- setdiff(names(coefficients), c("(Intercept)"))
  correct_predictors <- setdiff(names(coefficients[coefficients != 0]),
                                c("(Intercept)"))
  fitted <- Simulation$get_fitted_coefficients()[,predictor_names,] #w/o Intercept
  algo_names <-colnames(Simulation$get_test_sse())

  out <- sapply(algo_names, function(i) {
    sum(apply(fitted[,,i], MARGIN = 1, function(x) {
      all(sapply(correct_predictors, function(y) ! is.na(x[[y]])))
    }) / nrow(fitted)
    )
  })
  knitr::kable(out, align='c',
               caption = "% Simulations Selected at Minimum All
               Correct Covariates")
}
