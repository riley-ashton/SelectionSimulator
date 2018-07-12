context("Simulation")

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

test_obj_1 <- Simulation$new(sim_data_gen, c(Step2$new, Step3$new), 10, 1)

test_obj_1$simulate()
test_obj_1$get_inclusion_orders()[1,2]
test_obj_1$get_inclusion_orders()[1,1]

test_that("correct_mean", {
})


temp <- lapply(c(Step2$new, Step3$new), function(new) {
  new(data = sim_data_gen$simulate_data(), response_variable = sim_data_gen$get_response_name(),
      starting_formula = response ~ 1, "forward", k = 2)
})

