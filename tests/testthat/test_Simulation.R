context("Simulation")
set.seed(7122018)

cov_mat_5 <- rbind( c(1.00, -0.80,  0.75, 0.00, 0.00),
                    c(-0.80,  1.00, -0.25, 0.00, 0.00),
                    c( 0.75, -0.25,  1.00, 0.00, 0.00),
                    c( 0.00,  0.00,  0.00, 1.00, 0.25),
                    c( 0.00,  0.00,  0.00, 0.25, 1.00))

coefficients_1 <- c(V1 = 1, V2 = 2, V3 = 3, V4 = 4, V5 = -5)

predict_gen <- NormalPredictorsGenerator$new(num_observations = 100,
                                             num_predictors = 5,
                                             norm_rand_var_sd = 2,
                                             covariance_matrix = cov_mat_5,
                                             predictor_names = names(coefficients_1))


response_calc_1 <- LinearNormalResponseCalculator$new(norm_rand_var_sd = 1,
                                                      coefficients = coefficients_1,
                                                      intercept = 9)

sim_data_gen <- SimulationDataGenerator$new(predict_gen, response_calc_1)

test_obj_1 <- Simulation$new(sim_data_gen, c(Step = StepLmWrapper$new, Step2 = Step2$new, Step3 = Step3$new), 5, 1)

test_obj_1$simulate()


# Changes if algorithm changes.
# Checks that Step3 is including V1V2V3 together at least once
test_that("inclusion_order_dependson_Step3", {
  inclusion_orders_Step3 <- test_obj_1$get_inclusion_orders()[,"Step3"]
  triple_inclusions <- sapply(inclusion_orders_Step3, function(x) "V1V2V3" %in% x)
  expect_true(any(triple_inclusions))
})


test_that("fitted_coefficients", {
  X <- apply(test_obj_1$get_fitted_coefficients(), MARGIN = c(2,3), mean)
  expect_equal(mean(X["(Intercept)",]), 9, tolerance = 0.5)
  expect_equal(mean(X["V2",]), 2, tolerance = 0.5)
  expect_equal(mean(X["V3",]), 3, tolerance = 0.5)
  expect_equal(mean(X["V4",]), 4, tolerance = 0.5)
  expect_equal(mean(X["V5",]), -5, tolerance = 0.5)
})
