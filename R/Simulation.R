#' Class for Simulating stepwise
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @import parallel
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
#'   \item{\code{get_test_sse}}{Returns the sse on a new test set}
#'   \item{\code{get_training_sse}}{Returns the sse on the training set}
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
      continuous_response <- self$SimulationDataGenerator$
        ResponseCalculator$response_is_continuous()
      private$response_name <- self$SimulationDataGenerator$get_response_name()
      sim_data_names <- colnames(private$generate_sim_data())

      results <- parallel::mclapply(1:private$num_simulations, function(i) {
        sim_data <- private$generate_sim_data()
        step_objects <- private$stepwise_objects(sim_data)
        for(obj in step_objects) {
          obj$run()
        }

        if(continuous_response) {
          out <- list(
            sample_correlation = cor(sim_data),
            inclusion_order = private$compute_inclusion_order(step_objects),
            fitted_coefficients = private$compute_fitted_coefficients(step_objects),
            test_sse = private$compute_test_sse(sim_data, step_objects),
            training_sse = private$compute_training_sse(sim_data, step_objects)
          )
        } else {
          out <- list(
            sample_correlation = cor(sim_data),
            inclusion_order = private$compute_inclusion_order(step_objects),
            fitted_coefficients = private$compute_fitted_coefficients(step_objects),
            test_classification_rate = private$compute_test_classification_rate(sim_data, step_objects),
            traning_classification_rate = private$compute_training_classification_rate(sim_data, step_objects)
          )
        }
        out
      }, mc.cores = private$parallel_cores)

      private$combine_sample_correlation(sim_data_names, results)
      private$organize_fitted_coefficients(results)
      private$inclusion_orders <- t(sapply(results, function(obj) obj$inclusion_order))

      if(continuous_response) {
        private$test_sse <-  t(sapply(results, function(obj) obj$test_sse))
        private$training_sse <-  t(sapply(results, function(obj) obj$training_sse))
      } else {
        private$test_classification_rate <-  t(sapply(results, function(obj) obj$test_classification_rate))
        private$training_classification_rate <-  t(sapply(results, function(obj) obj$traning_classification_rate))
      }

      # Reduce object size
      private$stepwise_constructors = NULL
    },

    get_inclusion_orders = function() {
      private$inclusion_orders
    },

    get_fitted_coefficients = function() {
      private$fitted_coefficients
    },

    get_test_sse = function() {
      private$test_sse
    },

    get_training_sse = function() {
      private$training_sse
    },

    get_test_classification_rate = function() {
      private$test_classification_rate
    },

    get_training_classification_rate = function() {
      private$training_classification_rate
    },

    get_min_sample_correlation = function() {
      private$min_sample_correlation
    },

    get_max_sample_correlation = function() {
      private$max_sample_correlation
    }
  ),

  private = list(
    stepwise_constructors = NULL,
    num_simulations = NULL,
    inclusion_orders = NULL,
    fitted_coefficients = NULL,
    test_sse = NULL,
    training_sse = NULL,
    test_classification_rate = NULL,
    training_classification_rate = NULL,
    sim_data = NULL,
    response_name = NULL,

    # A matrix of the minimum values seen for sample correlations
    min_sample_correlation = NULL,
    # A matrix of the maximum values seen for sample correlations
    max_sample_correlation = NULL,
    # Number of CPU cores to use in a simulation
    parallel_cores = NULL,


    # Generates the data for a simulation
    generate_sim_data = function() {
      private$sim_data <- self$SimulationDataGenerator$simulate_data()
    },


    # Creates the stepwise objects from the stepwise algorithms
    stepwise_objects = function(sim_data) {
      starting_formula <- as.formula(paste0(private$response_name, "~ 1"))

      lapply(private$stepwise_constructors, function(new) {
        new(data = sim_data, response_variable = private$response_name,
            starting_formula, "forward",
            k = log(self$SimulationDataGenerator$PredictorsGenerator$get_num_observations()))
      })
    },


    # Returns the fitted coefficients for each stepwise algorithm for a given simulation
    compute_fitted_coefficients = function(step_objects) {
      lapply(step_objects, function(obj) {
        fitted_model <- obj$get_fitted_model()
        fitted_model$coefficients
      })
    },

    # Returns the inclusion order for each stepwise algorithm for a given simulation
    compute_inclusion_order = function(step_objects) {
      lapply(step_objects, function(obj) {
        inclusion_order <- obj$get_inclusion_order()
        if(length(inclusion_order) == 0) { #avoid NULL
          return("")
        } else {
          return(inclusion_order)
        }
      })
    },

    # Computes and returns the training sse for each stepwise algorithm for a given simulation
    compute_training_sse = function(sim_data, step_objects) {
      predictor_names <- setdiff(colnames(sim_data), private$response_name)
      predictors <- sim_data[,predictor_names]
      response <- sim_data[private$response_name]

      out <- sapply(step_objects, function(obj) {
        fitted_model <- obj$get_fitted_model()
        predicted_response <- predict(fitted_model, predictors)
        sum((predicted_response - response) ^ 2)
      })
      out
    },


    # Computes and returns the test sse for each stepwise algorithm for a given simulation
    compute_test_sse = function(sim_data, step_objects) {
      predictor_names <- setdiff(colnames(sim_data), private$response_name)
      test_data <- self$SimulationDataGenerator$simulate_data()
      predictors <- test_data[,predictor_names]
      response <- test_data[private$response_name]

      out <- sapply(step_objects, function(obj) {
        fitted_model <- obj$get_fitted_model()
        predicted_response <- predict(fitted_model, predictors)
        sum((predicted_response - response) ^ 2)
      })
      out
    },


    # Computes and returns the test classification rate for each stepwise algorithm for a given simulation
    compute_training_classification_rate = function(sim_data, step_objects) {
      sapply(step_objects, function(obj) {
        response_index <- match(private$response_name, colnames(sim_data))
        fitted <- predict(obj$get_fitted_model(), sim_data[, -response_index], type='response')
        fitted <- ifelse(fitted > 0.5, 1, 0)
        response <- sim_data[, response_index]
        mis_class_error <- mean(fitted != response)
        1 - mis_class_error
      })
    },


    # Computes and returns the training classification rate for each stepwise algorithm for a given simulation
    compute_test_classification_rate = function(sim_data, step_objects) {
      sapply(step_objects, function(obj) {
        test_data <- self$SimulationDataGenerator$simulate_data()
        response_index <- match(private$response_name, colnames(sim_data))
        fitted <- predict(obj$get_fitted_model(), test_data[,-response_index], type='response')
        fitted <- ifelse(fitted > 0.5, 1, 0)
        response <- sim_data[,response_index]
        mis_class_error <- mean(fitted != response)
        1 - mis_class_error
      })
    },


    # Put elementwise maxes and minimums from all the sample correlation matrices,
    #    into private$max_sample_correlation and private$min_sample_correlation respectively
    combine_sample_correlation = function(sim_data_names, results) {
      samples_cors <- lapply(results, function(obj) obj$sample_correlation)
      maxes <- Reduce(pmax, samples_cors, -1)
      mins <- Reduce(pmin, samples_cors, 1)

      private$min_sample_correlation <- matrix(mins, nrow = length(sim_data_names))
      colnames(private$min_sample_correlation) <- sim_data_names
      row.names(private$min_sample_correlation) <- sim_data_names

      private$max_sample_correlation <- matrix(maxes, nrow = length(sim_data_names))
      colnames(private$max_sample_correlation) <- sim_data_names
      row.names(private$max_sample_correlation) <- sim_data_names
    },

    # Takes the unorganized list<list<vector<double>>> from fitted_coefficients
    # and organizes the fitted coefficients into a 3D array
    organize_fitted_coefficients = function(results) {
      fitted_coefficients <- sapply(results, function(obj) obj$fitted_coefficients)
      # Get and sort coefficient names
      coefficient_names <- unique(names(unlist(fitted_coefficients)))
      coefficient_names <- gtools::mixedsort(coefficient_names)

      # If present put (Intercept) at the beginning
      if("(Intercept)" %in% coefficient_names) {
        index <- match("(Intercept)", coefficient_names)
        coefficient_names <- c("(Intercept)", coefficient_names[-index])
      }

      # Allocate array
      nrows <- private$num_simulations
      ncols <- length(coefficient_names)
      num_algos <- nrow(fitted_coefficients)

      organized_coefficients <- array(dim = c(nrows, ncols, num_algos))
      colnames(organized_coefficients) <- coefficient_names
      dimnames(organized_coefficients)[[3]] <- rownames(fitted_coefficients)

      # Sort coefficients into correct location
      for(algo_num in 1:num_algos) {
        for(row in 1:nrows) {
          for(name in coefficient_names) {
            coefficient <- tryCatch(fitted_coefficients[[algo_num, row]][[name]],
                                    error = function(e) NA)
            organized_coefficients[[row, name, algo_num]] <- unname(coefficient)
          }
        }
      }
      private$fitted_coefficients <- organized_coefficients
    }
  )
)
