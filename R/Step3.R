#' Class/algorithm for Stepwise regression that considers correlation structure (recursively)
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object
#' @field PredictorsGenerator object for generating predictors
#' @field ResponseCalculator object for computing response
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor}
#'   \item{\code{run}}{Runs the stepwise regression and saves the fitted model and the order the covariates were included}
#'   \item{\code{get_fitted_model}}{Returns the glm or lm object (must call "run()" first)}
#'   \item{\code{get_inclusion_order}}{Returns the order covariates were included in (list<vector<string>>) (must call "run()" first)}
#'   }

Step3 <- R6::R6Class("Step3",
   inherit = Step2,
   public = list(

     # Hash table for determining whether a predictor_block is already
     # present.
     #
     # predictor_blocks are added to self$predictor_blocks and here by
     # the recurse_on_predictor_blocks algorithm, using the helper functions
     # in_block_present_table and add_block_to_present_table.
     #
     # block_present_table is used for the hash tables O(1) lookup time
     block_present_table = NULL,

     # List of vectors, indexed by variable name "X", which contain a
     # vector of variables that surpass the correlation threshold with respect
     # to "X"
     #
     # Example
     # index  vector
     # "V1"   c("V2", "V4")
     # "V2"   c("V1", "V4")
     # "V3"   c()
     # "V4"   c("V1", "V2")
     cor_list = NULL,

     recursive_cor_positive_cutoff = NULL,
     recursive_cor_negative_cutoff = NULL,
     max_block_size = NULL,

     initialize = function(data, response_variable, starting_formula,
                           stepwise_direction, k, glm_family = gaussian,
                           print_trace = FALSE, cor_cutoff = -0.5,
                           recursive_cor_negative_cutoff = -0.5,
                           recursive_cor_positive_cutoff = 0.5,
                           max_block_size = 3) {
          super$initialize(data, response_variable, starting_formula,
                           stepwise_direction, k, glm_family,
                           print_trace, cor_cutoff)

          self$recursive_cor_negative_cutoff <- recursive_cor_negative_cutoff
          self$recursive_cor_positive_cutoff <- recursive_cor_positive_cutoff
          self$max_block_size <- max_block_size

          self$generate_block_present_table()
          self$generate_cor_list()
          self$recurse_on_predictor_blocks()
     },

     generate_block_present_table = function() {
       # Create hashtable
       table_size <- 5 * length(self$predictor_blocks)
       self$block_present_table <- new.env(hash = TRUE, size = table_size)

       for(block in self$predictor_blocks) {
         self$add_block_to_present_table(block)
       }
     },

     # Generates list of vectors, indexed by variable name "X", which contain a
     # vector of variables that surpass the correlation threshold with respect
     # to "X"
     #
     # Example
     # index  vector
     # "V1"   c("V2", "V4")
     # "V2"   c("V1", "V4")
     # "V3"   c()
     # "V4"   c("V1", "V2")
     generate_cor_list = function() {
       covariate_data <- self$data[self$covariate_names]
       covariate_correlation <- cor(covariate_data)
       covariate_test <- covariate_correlation < self$recursive_cor_negative_cutoff |
                        covariate_correlation > self$recursive_cor_positive_cutoff

       self$cor_list <- lapply(self$covariate_names, function(cov) {
         x <- covariate_test[cov,]
         y <- names(x)[x]
         setdiff(y, cov)
       })
       names(self$cor_list) <- self$covariate_names
     },

     # Examines all the predictor_blocks and adds new blocks that contain any
     # covariates highly positively or negatively related any of the covariates
     # already in the predictor_block.
     # Uses helper function append_singles_to_blocks, which acts recursively
     recurse_on_predictor_blocks = function() {
       for(block in self$predictor_blocks) {
         checked_vars <- c(FALSE, FALSE)
         names(checked_vars) <- block
         self$append_singles_to_blocks(block, checked_vars)
       }
       self$predictor_blocks <- unique(self$predictor_blocks)
     },

     # Helper function to recurse_on_predictor_blocks
     append_singles_to_blocks = function(block, checked_vars) {
       for(var in block) {
         if(! checked_vars[[var]]) {
           checked_vars[[var]] <- TRUE
           for(cor_var in self$cor_list[[var]]) {
             if(! cor_var %in% block) {
               new_block <- c(block, c(cor_var))
               new_block <- sort(new_block)
               if(! self$in_block_present_table(new_block)) {
                 self$add_block_to_present_table(new_block)
                 self$predictor_blocks <- c(self$predictor_blocks, list(new_block))
                 checked_vars[[cor_var]] <-  FALSE
                 if(length(new_block) < self$max_block_size) {
                   self$append_singles_to_blocks(new_block, checked_vars)
                 }
               }
             }
           }
         }
       }
     },

     # Checks whether a block is in the block_present_table hash table
     in_block_present_table = function(block) {
       lookup_id <- Reduce(paste0, block)
       ! is.null(self$block_present_table[[lookup_id]])
     },

     # Adds a block to the block_present_table hash table
     add_block_to_present_table = function(block) {
       lookup_id <- Reduce(paste0, block)
       self$block_present_table[[lookup_id]] <- TRUE
     }
     ),
   private = list()
)
