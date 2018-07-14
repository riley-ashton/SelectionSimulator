StepLmWrapper <- R6::R6Class("StepLmWrapper",
  public = list(
    initialize = function(data, response_variable,
                          starting_formula, stepwise_direction,
                          k) {
      private$data <- data
      private$response_variable <- response_variable
      private$starting_formula <- starting_formula
      private$stepwise_direction <- stepwise_direction
      private$k <- k
    },

    get_fitted_model = function() {
      private$fitted_model
    },

    get_inclusion_order = function() {
      private$inclusion_order
    },

    run = function() {
      full <- formula(lm(as.formula(paste0(private$response_variable, "~ .")), data = private$data))
      private$fitted_model <- step(lm(as.formula(paste0(private$response_variable, "~ 1")), data = private$data),
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
    stepwise_direction = NULL
  )
)
