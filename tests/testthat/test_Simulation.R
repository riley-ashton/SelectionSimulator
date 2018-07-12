context("Simulation")
set.seed(7122018)

cov_mat_5 <- rbind( c(1.00, -0.80,  0.75, 0.00, 0.00),
                    c(-0.80,  1.00, -0.25, 0.00, 0.00),
                    c( 0.75, -0.25,  1.00, 0.00, 0.00),
                    c( 0.00,  0.00,  0.00, 1.00, 0.25),
                    c( 0.00,  0.00,  0.00, 0.25, 1.00))

predict_gen <- NormalPredictorsGenerator$new(num_observations = 100,
                                             num_predictors = 5,
                                             norm_rand_var_sd = 2,
                                             covariance_matrix = cov_mat_5)

coefficients_1 <- c(V1 = 1, V2 = 2, V3 = 3, V4 = 4, V5 = -5)

response_calc_1 <- LinearNormalResponseCalculator$new(norm_rand_var_sd = 1,
                                                      coefficients = coefficients_1,
                                                      intercept = 9)

sim_data_gen <- SimulationDataGenerator$new(predict_gen, response_calc_1)

test_obj_1 <- Simulation$new(sim_data_gen, c(Step2 = Step2$new, Step3 = Step3$new), 5, 1)

test_obj_1$simulate()


# Changes if algorithm changes
test_that("inclusion_order_dependson_Step3", {
})


test_that("fitted_coefficients", {
  fitted_coefficients <-test_obj_1$get_fitted_coefficients()
  names <- names(fitted_coefficients[[1,1]])

  step2_average_fitted_coefficients <- sapply(names, function(name) {
    Reduce(function(y, x) x[name] / 5 + y, fitted_coefficients[,1], init = 0)
  })
  names(step2_average_fitted_coefficients) <- names


  step3_average_fitted_coefficients <- sapply(names, function(name) {
    Reduce(function(y, x) x[name] / 5 + y, fitted_coefficients[,2], init = 0)
  })
  names(step3_average_fitted_coefficients) <- names


  expected_coefficients <- c(`(Intercept)` = 9, V5 = -5, V4 = 4,
                             V1 = 1, V2 = 2, V3 = 3)


  expect_equal(step2_average_fitted_coefficients,
               expected_coefficients, tolerance = 0.5)

  expect_equal(step3_average_fitted_coefficients,
               expected_coefficients, tolerance = 0.5)
})

