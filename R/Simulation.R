Simulation <- R6::R6Class("Simulation",
  public = list (
    SimulationDataGenerator = NULL,


    initialize = function(SimulationDataGenerator, stepwise_constructors,
                          num_simulations, parallel_cores) {
      self$SimulationDataGenerator <- SimulationDataGenerator
      private$stepwise_constructors <- stepwise_constructors
      private$num_simulations <- num_simulations
      private$parallel_cores <- parallel_cores
    },

    simulate = function() {
      private$preallocate_lists()
      continuous_response <- self$SimulationDataGenerator$ResponseCalculator$response_is_continuous()

      for(i in seq.int(private$num_simulations)) {
        private$generate_sim_data()
        private$construct_stepwise_objects()
        private$call_run_on_stepwise_objects()
        private$add_fitted_coefficients_at_index(i)
        private$add_inclusion_order_at_index(i)

        if(continuous_response) {
          private$add_test_mse_at_index(i)
          private$add_training_mse_at_index(i)
        } else {
          private$add_test_classification_rate_at_index(i)
          private$add_training_classification_rate_at_index(i)
        }
      }
    },

    get_inclusion_orders = function() {
      private$inclusion_orders
    },

    get_fitted_coefficients = function() {
      private$fitted_coefficients
    },

    get_test_mse = function() {
      private$test_mse
    },

    get_training_mse = function() {
      private$training_mse
    },

    get_test_classification_rate = function() {
      private$test_classification_rate
    },

    get_training_classification_rate = function() {
      private$training_classification_rate
    }
  ),

  private = list(
    stepwise_constructors = NULL,
    num_simulations = NULL,
    parallel_cores = NULL,
    inclusion_orders = NULL,
    fitted_coefficients = NULL,
    test_mse = NULL,
    training_mse = NULL,
    test_classification_rate = NULL,
    training_classification_rate = NULL,
    sim_data = NULL,
    stepwise_objects = NULL,

    preallocate_lists = function() {
      num_columns <- length(private$stepwise_constructors)
      num_rows <- private$num_simulations
      stepwise_names <- names(private$stepwise_constructors)

      private$inclusion_orders <- replicate(num_columns, vector(mode = "list", length = num_rows))
      colnames(private$inclusion_orders) <- stepwise_names

      private$fitted_coefficients <- replicate(num_columns, vector(mode = "list", length = num_rows))
      colnames(private$fitted_coefficients) <- stepwise_names

      private$test_mse <- replicate(num_columns, vector(mode = "numeric", length = num_rows))
      colnames(private$test_mse) <- stepwise_names

      private$training_mse <- replicate(num_columns, vector(mode = "numeric", length = num_rows))
      colnames(private$training_mse) <- stepwise_names
    },

    generate_sim_data = function() {
      private$sim_data <- self$SimulationDataGenerator$simulate_data()
    },

    construct_stepwise_objects = function() {
      response_name <- self$SimulationDataGenerator$get_response_name()
      starting_formula <- as.formula(paste0(response_name, "~ 1"))

      k <- log(self$SimulationDataGenerator$PredictorsGenerator$get_num_observations())
      private$stepwise_objects <- lapply(private$stepwise_constructors, function(new) {
        new(data = private$sim_data, response_variable = response_name, starting_formula, "forward", k)
      })
    },

    call_run_on_stepwise_objects = function() {
      for(obj in private$stepwise_objects) {
        obj$run()
      }
    },

    add_fitted_coefficients_at_index = function(i) {
      stepwise_indices <- seq_along(private$stepwise_objects)
      for(j in stepwise_indices) {
        fitted_model <- private$stepwise_objects[[j]]$get_fitted_model()
        private$fitted_coefficients[[i, j]] <- fitted_model$coefficients
      }
    },

    add_inclusion_order_at_index = function(i) {
      stepwise_indices <- seq_along(private$stepwise_objects)
      for(j in stepwise_indices) {
        inclusion_order <- private$stepwise_objects[[j]]$get_inclusion_order()
        private$inclusion_orders[[i, j]] <- inclusion_order
      }
    },

    add_test_mse_at_index = function(i) {

    },

    add_training_mse_at_index = function(i) {

    },

    add_test_classification_rate_at_index = function(i) {

    },

    add_training_classification_rate_at_index = function(i) {

    }
  )
)
