Simulation <- R6Class("Simulation2tibble",
  public = list (
    SimulationDataGenerator = NULL,


    initialize = function(SimulationDataGenerator, stepwise_algos,
                          num_simulations, parallel_cores) {
      self$SimulationDataGenerator <- SimulationDataGenerator
      private$stepwise_algos <- stepwise_algos
      private$num_simulations <- num_simulations
      private$parallel_cores <- parallel_cores
    },

    simulate = function() {
      private$preallocate_lists()
      response_name <- self$SimulationDataGenerator$get_response_name()

      for(i in seq.int(private$num_simulations)) {
        sim_data <- self$SimulationDataGenerator$simulate_data()
        # TODO
      }
    },

    get_inclusion_orders = function() {
      private$inclusion_orders
    },

    get_fitted_coefficients = function() {
      private$fitted_coefficients
    },

    get_predictive_power = function() {
      private$predictive_power
    }
  ),
  private = list(
    stepwise_algos = NULL,
    num_simulations = NULL,
    parallel_cores = NULL,
    test_holdout_values = NULL,
    inclusion_orders = NULL,
    fitted_coefficients = NULL,
    predictive_power = NULL,

    preallocate_lists = function() {
      private$inclusion_orders = vector(mode = "list", length = private$num_simulations)
      private$fitted_coefficients = vector(mode = "list", length = private$num_simulations)
      private$predictive_power = vector(mode = "numeric", length = private$num_simulations)
    }
  )
)
