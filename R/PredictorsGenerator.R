###
# Predictors Generator Abstract Class
###

library(R6)

PredictorsGenerator <- R6Class("PredictorsGenerator",
   public = list(
     initialize() {
       stop("Abstract Class! Cannot initialize!")
     }
     get_num_observations = function() {
       private$num_observations
     },
     get_num_predictors = function() {
       private$num_predictors
     }
   ),
   private = list(
     num_observations = NULL,
     num_predictors = NULL
   )
)
