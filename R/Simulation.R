#' Class for Simulating stepwise
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @field SimulationDataGenerator R6 object for simulating data
#' @field ResponseCalculator object for computing response
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor}
#'   \item{\code{simulate}}{Runs stepwise algorithms}
#'   \item{\code{get_inclusion_orders}}{Returns the order of inclusions}
#'   \item{\code{get_fitted_coefficients}}{Returns the fitted coefficients}
#'   \item{\code{get_test_mse}}{Returns the mse on a new test set}
#'   \item{\code{get_training_mse}}{Returns the mse on the training set}
#'   \item{\code{get_test_classification_rate}}{Returns the classification rate on a new test set}
#'   \item{\code{get_training_classification_rate}}{Returns the classification rate on the training set}
#'   }

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
      private$continuous_response <- self$SimulationDataGenerator$
        ResponseCalculator$response_is_continuous()
      private$preallocate_lists()

      for(i in 1:private$num_simulations) {
        private$generate_sim_data()
        private$construct_stepwise_objects()
        private$call_run_on_stepwise_objects()
        private$add_fitted_coefficients_at_index(i)
        private$add_inclusion_order_at_index(i)

        if(private$continuous_response) {
          private$add_test_mse_at_index(i)
          private$add_training_mse_at_index(i)
        } else {
          private$add_test_classification_rate_at_index(i)
          private$add_training_classification_rate_at_index(i)
        }
      }

      private$organize_fitted_coefficients()

      private$sim_data = NULL
      private$stepwise_objects = NULL
      private$stepwise_constructors = NULL
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
    continuous_response = NULL,
    response_name = NULL,

    preallocate_lists = function() {
      num_columns <- length(private$stepwise_constructors)
      num_rows <- private$num_simulations
      stepwise_names <- names(private$stepwise_constructors)

      private$inclusion_orders <- replicate(num_columns,
                                            vector(mode = "list", length = num_rows))
      colnames(private$inclusion_orders) <- stepwise_names

      private$fitted_coefficients <- replicate(num_columns,
                                        vector(mode = "list", length = num_rows))
      colnames(private$fitted_coefficients) <- stepwise_names

      if(private$continuous_response) {
        private$allocate_continuous_response_lists()
      } else {
        private$allocate_discrete_response_lists()
      }
    },

    allocate_continuous_response_lists = function() {
      num_columns <- length(private$stepwise_constructors)
      num_rows <- private$num_simulations
      stepwise_names <- names(private$stepwise_constructors)

      private$test_mse <- replicate(num_columns,
                                    vector(mode = "numeric", length = num_rows))
      colnames(private$test_mse) <- stepwise_names

      private$training_mse <- replicate(num_columns,
                                        vector(mode = "numeric", length = num_rows))
      colnames(private$training_mse) <- stepwise_names
    },

    allocate_discrete_response_lists = function() {
      num_columns <- length(private$stepwise_constructors)
      num_rows <- private$num_simulations
      stepwise_names <- names(private$stepwise_constructors)

      private$test_classification_rate <- replicate(num_columns,
                                                    vector(mode = "numeric", length = num_rows))
      colnames(private$test_classification_rate) <- stepwise_names

      private$training_classification_rate <- replicate(num_columns,
                                                        vector(mode = "numeric", length = num_rows))
      colnames(private$training_classification_rate) <- stepwise_names
    },

    generate_sim_data = function() {
      private$sim_data <- self$SimulationDataGenerator$simulate_data()
    },

    construct_stepwise_objects = function() {
      private$response_name <- self$SimulationDataGenerator$get_response_name()
      starting_formula <- as.formula(paste0(private$response_name, "~ 1"))

      private$stepwise_objects <- lapply(private$stepwise_constructors, function(new) {
        new(data = private$sim_data, response_variable = private$response_name,
            starting_formula, "forward",
            k = log(self$SimulationDataGenerator$PredictorsGenerator$get_num_observations()))
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

    add_training_mse_at_index = function(i) {
      predictor_names <- setdiff(colnames(private$sim_data), private$response_name)
      predictors <- private$sim_data[,predictor_names]
      response <- private$sim_data[private$response_name]

      stepwise_indices <- seq_along(private$stepwise_objects)

      for(j in stepwise_indices) {
        fitted_model <- private$stepwise_objects[[j]]$get_fitted_model()

        predicted_response <- predict(fitted_model, predictors)
        private$training_mse[[i,j]] <- sum((predicted_response - response) ^ 2)
      }
    },

    add_test_mse_at_index = function(i) {
      stepwise_indices <- seq_along(private$stepwise_objects)

      predictor_names <- setdiff(colnames(private$sim_data), private$response_name)
      test_data <- self$SimulationDataGenerator$simulate_data()
      predictors <- test_data[,predictor_names]
      response <- test_data[private$response_name]

      for(j in stepwise_indices) {
        fitted_model <- private$stepwise_objects[[j]]$get_fitted_model()

        predicted_response <- predict(fitted_model, predictors)
        private$test_mse[[i,j]] <- sum((predicted_response - response) ^ 2)
      }
    },

    add_test_classification_rate_at_index = function(i) {
      stop("Not yet implemented")
    },

    add_training_classification_rate_at_index = function(i) {
      stop("Not yet implemented")
    },

    # Takes the unorganized list<list<vector<double>>> from fitted_coefficients
    # and organizes the fitted coefficients into a 3D array
    organize_fitted_coefficients = function() {

      # Get and sort coefficient names
      coefficient_names <- unique(names(unlist(private$fitted_coefficients)))
      coefficient_names <- gtools::mixedsort(coefficient_names)

      # If present put (Intercept) at the beginning
      if("(Intercept)" %in% coefficient_names) {
        index <- match("(Intercept)", coefficient_names)
        coefficient_names <- c("(Intercept)", coefficient_names[-index])
      }

      # Allocate array
      nrows <- private$num_simulations
      ncols <- length(coefficient_names)
      ndepth <- length(private$stepwise_objects)


      organized_coefficients <- array(dim = c(nrows, ncols, ndepth))
      colnames(organized_coefficients) <- coefficient_names
      dimnames(organized_coefficients)[[3]] <- names(private$stepwise_objects)

      # Sort coefficients into correct location
      for(depth in seq.int(ndepth)) {
        for(row in seq.int(nrows)) {
          for(name in coefficient_names) {
            coefficient <- tryCatch(private$fitted_coefficients[[row, depth]][[name]],
                                    error = function(e) NA)
            organized_coefficients[[row, name, depth]] <- unname(coefficient)
          }
        }
      }
      private$fitted_coefficients <- organized_coefficients
    }

  )
)
