context("SimulationDataGenerator")

#####################
# PredictorsGenerator
#####################
cov_mat_2 <- rbind( c(1.00, -0.80),
                    c(-0.80,  1.00))

predictors_object <- NormalPredictorsGenerator$new(num_observations = 10000,
                                                   num_predictors = 2,
                                                   norm_rand_var_sd = 2,
                                                   covariance_matrix = cov_mat_2)

##############################################
# ResponseCalculator from Linear gaussian model
###############################################
V1 <- 10 * runif(1000)
V2 <- 10 * runif(1000)
z <- 2 * V1 + 7 * V2 + 15
t1 <- data.frame(V1, V2, z)


response_object <- LinearNormalResponseCalculator$new(norm_rand_var_sd = 2,
                                                      coefficients = c(V1 = 2, V2 = 7),
                                                      intercept = 15)

#######
# Tests
#######
obj1 <- SimulationDataGenerator$new(PredictorsGenerator = predictors_object,
                            ResponseCalculator = response_object)

simulated_data <- obj1$simulate_data()
irreducible_error <- with(simulated_data, response - 2 * V1 - 7 * V2 - 15)

test_that("dimensions_as_expected", {
  expect_equal(dim(simulated_data), c(10000, 3))
})

test_that("sd_as_expected", {
  expect_equal(sd(irreducible_error), 2, tolerance = 0.05)
})

test_that("mean_as_expected", {
  expect_equal(sum(irreducible_error) / 10000, 0, tolerance = 0.05)
})

