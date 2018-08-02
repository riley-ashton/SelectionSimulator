context("LogisticResponseCalculator")

###################################
# Normal continuous predictors test
###################################
cov_mat <- rbind( c(1.00, -0.80),
                    c(-0.80,  1.00))

coefficients_1 <- c(V1 = 1, V2 = -2)

predict_gen <- NormalPredictorsGenerator$new(num_observations = 5,
                                             num_predictors = 2,
                                             norm_rand_var_sd = 2,
                                             covariance_matrix = cov_mat,
                                             predictor_names = names(coefficients_1))


response_calc_1 <- LogisticResponseCalculator$new(coefficients = coefficients_1,
                                                      intercept = 2)

predictors <- predict_gen$simulate_predictors()

computed_response <- replicate(1000, response_calc_1$calculate_response(predictors))
computed_response_means <- apply(computed_response, MARGIN = 1, mean)

expected_XB <- as.matrix(predictors) %*% coefficients_1 + 2
expected <- as.vector(exp(expected_XB) / (1 + exp(expected_XB)))

test_that("correct_means", {
  expect_equal(computed_response_means, expected, tolerance = 0.05)
})

