Step2 <- R6::R6Class("Step2",
 public = list(
   data = NULL,
   response_variable = NULL,
   k = NULL,
   covariate_names = NULL,
   predictor_blocks = NULL,
   predictor_strings = NULL,
   current_predictors = NULL,
   stepwise_direction = NULL,
   current_formula = NULL,
   next_formulas = NULL,
   current_info_crit = NULL,
   past_formulas = NULL,
   print_trace = NULL,
   cor_cutoff = NULL,
   fitted_model = NULL,
   inclusion_order = NULL,

   initialize = function(data, response_variable,
                         starting_formula, stepwise_direction,
                         k, print_trace = FALSE, cor_cutoff = -0.5) {
     self$data <- data
     self$response_variable <- response_variable
     self$current_formula <- starting_formula
     self$stepwise_direction <- stepwise_direction
     self$k <- k
     self$print_trace <- print_trace
     self$cor_cutoff <- cor_cutoff
     self$current_info_crit <- extractAIC(lm(self$current_formula, self$data),
                                          scale = 0, k = self$k)[2]
     self$covariate_names <- setdiff(names(self$data), self$response_variable)
     self$compute_predictor_blocks()
     self$past_formulas <- new.env(hash = TRUE)
   },

   get_fitted_model = function() {
     self$fitted_model
   },

   get_inclusion_order = function() {
      self$inclusion_order
   },

   run = function() {
     while(TRUE) {
       self$compute_next_formulas()

       # Fitted full (forwards) or empty (backwards) model
       if(length(self$next_formulas) == 0) {
         self$fitted_model <- lm(self$current_formula, self$data)
         return()
       }

       compute_AIC <- function(x) extractAIC(lm(x, self$data),
                                             scale = 0, k = self$k)[2]
       info_crit <- sapply(self$next_formulas, compute_AIC)

       if(self$print_trace){
         print(rbind(as.character(self$next_formulas), info_crit))
       }

       min_info_crit <- min(info_crit)
       min_info_crit_index <- match(min_info_crit, info_crit)
       min_formula <- self$next_formulas[[min_info_crit_index]]

       if(! is.null(self$past_formulas[[as.character(min_formula)[3]]])) {
         stop("Cycle Present")
       }
       else if(min_info_crit < self$current_info_crit) {
         self$append_to_inclusion_order(min_formula)

         self$past_formulas[[as.character(self$current_formula)[3]]] <- 1
         self$current_info_crit <- min_info_crit
         self$current_formula <- min_formula
       }
       else if(min_info_crit > self$current_info_crit) {
         self$fitted_model <- lm(self$current_formula, self$data)
         return()
       }
       else if(min_info_crit == self$current_info_crit) {
         stop("Identical Information Criterions")
       }
     }
   },

   add_singles = function() {
     for(x in setdiff(self$covariate_names, self$current_predictors)) {
       to_append <- list(c(self$current_predictors, x))
       self$predictor_strings <- c(self$predictor_strings, to_append)
     }
   },

   add_blocks = function() {
     for(i in seq_along(self$predictor_blocks)) {
       block <- self$predictor_blocks[[i]]
       not_in_predictors <- function(x){ ! (x %in% self$current_predictors) }
       predictors_not_present <- sapply(block, not_in_predictors)

       if(all(predictors_not_present)) {
         to_append <- list(union(self$current_predictors, self$predictor_blocks[[i]]))
         self$predictor_strings <- c(self$predictor_strings, to_append)
       }
     }
   },


   remove_singles = function() {
     for(x in self$current_predictors) {
       to_append <- list(Filter(function(y) y != x, self$current_predictors))
       self$predictor_strings <- c(self$predictor_strings, to_append)
     }
   },

   remove_blocks = function() {
     for(i in seq_along(self$predictor_blocks)) {
       block <- self$predictor_blocks[[i]]
       in_predictors <- function(x) x %in% self$current_predictors
       predictors_present <- sapply(block, in_predictors)
       if(all(predictors_present)) {
         to_append <- list(setdiff(self$current_predictors, self$predictor_blocks[[i]]))
         self$predictor_strings <- c(self$predictor_strings, to_append)
       }
     }
   },

   compute_next_formulas = function() {
     self$predictor_strings <- list()
     self$current_predictors <- strsplit(as.character(self$current_formula)[3],
                                         "+", fixed = TRUE)
     self$current_predictors <- sapply(self$current_predictors, trimws)
     if(self$stepwise_direction == "forward" | self$stepwise_direction == "both") {
       self$add_singles()
       self$add_blocks()
     }
     if(self$stepwise_direction == "backward" | self$stepwise_direction == "both") {
       self$remove_singles()
       self$remove_blocks()
     }
     self$next_formulas <- sapply(self$predictor_strings, function(z) {
       rhs <- Reduce(function(x,y) paste(x, y, sep = " + "), z)
       lhs <- as.character(self$current_formula)[2]
       as.formula(paste(lhs, rhs, sep = " ~ "))
     })
   },


   compute_predictor_blocks = function() {
     covariate_data <- self$data[self$covariate_names]
     self$predictor_blocks <- list()

     covariate_test <- cor(covariate_data) < self$cor_cutoff

     for(row in 2:length(self$covariate_names)) {
       for(col in 1:(row - 1)) {
         if(covariate_test[row, col]) {
           to_append <- list(c(self$covariate_names[col], self$covariate_names[row]))
           self$predictor_blocks <- c(self$predictor_blocks, to_append)
         }
       }
     }
   },

 append_to_inclusion_order = function(min_formula) {
   existing_vars <- strsplit(as.character(self$current_formula)[3], "+", fixed = TRUE)
   existing_vars <- sapply(existing_vars, trimws)

   current_vars <- strsplit(as.character(min_formula)[3], "+", fixed = TRUE)
   current_vars <- sapply(current_vars, trimws)

   new_vars <- setdiff(current_vars, existing_vars)
   to_append <- Reduce(paste0, new_vars)
   self$inclusion_order <- c(self$inclusion_order, to_append)
 }
 ),
 private = list()
)
