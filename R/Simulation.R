Simulation <- R6Class("Simulation2tibble",
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

      for(i in seq.int(private$num_simulations)) {
        private$sim_data <- self$SimulationDataGenerator$simulate_data()
        #private$construct_stepwise_objects(sim_data, )


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
    stepwise_constructors = NULL,
    num_simulations = NULL,
    parallel_cores = NULL,
    test_holdout_values = NULL,
    inclusion_orders = NULL,
    fitted_coefficients = NULL,
    predictive_power = NULL,

    sim_data = NULL,
    stepwise_objects = NULL,

    construct_stepwise_objects = function() {
      response_name <- self$SimulationDataGenerator$get_response_name()
      starting_formula <- as.formula(paste0(response_name, "~ 1"))

      k <- log(self$SimulationDataGenerator$PredictorsGenerator$get_num_observations())
      lapply(stepwise_constructors,
             function(new) new(sim_data, response_name, starting_formula,
                               stepwise_direction = "forward", k)
      )
    },

    preallocate_lists = function() {
      private$inclusion_orders = vector(mode = "list", length = private$num_simulations)
      private$fitted_coefficients = vector(mode = "list", length = private$num_simulations)
      private$predictive_power = vector(mode = "numeric", length = private$num_simulations)
    }
  )
)
