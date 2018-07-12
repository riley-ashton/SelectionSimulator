context("LinearNormalResponseCalculator")

###################################
# Normal continuous predictors test
###################################
cov_mat_5 <- rbind( c(1.00, -0.80,  0.75, 0.00, 0.00),
                    c(-0.80,  1.00, -0.25, 0.00, 0.00),
                    c( 0.75, -0.25,  1.00, 0.00, 0.00),
                    c( 0.00,  0.00,  0.00, 1.00, 0.25),
                    c( 0.00,  0.00,  0.00, 0.25, 1.00))

predict_gen <- NormalPredictorsGenerator$new(num_observations = 1000,
                                      num_predictors = 5,
                                      norm_rand_var_sd = 2,
                                      covariance_matrix = cov_mat_5)

coefficients_1 <- c(V1 = 1, V2 = 2, V3 = 3, V4 = 4, V5 = -5)

response_calc_1 <- LinearNormalResponseCalculator$new(norm_rand_var_sd = 1,
                                                  coefficients = coefficients_1,
                                                  intercept = 9)

predictors <- predict_gen$simulate_predictors()

computed_response <- response_calc_1$calculate_response(predictors)

without_error_expected <- as.matrix(predictors) %*% coefficients_1 + 9


test_that("correct_mean", {
  expect_equal(sum(computed_response - without_error_expected), 0, tolerance = 5)
})

test_that("correct_sd", {
  expect_equal(sd(computed_response - without_error_expected), 1, tolerance = .1)
})

test_that("correct_dimensions", {
  expect_equal(length(computed_response), 1000)
})



###########################################
# Categorical and continous predictors test
###########################################

