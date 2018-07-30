context("NormalPredictorsGenerator")
cov_mat_5 <- rbind( c(1.00, -0.80,  0.75, 0.00, 0.00),
                    c(-0.80,  1.00, -0.25, 0.00, 0.00),
                    c( 0.75, -0.25,  1.00, 0.00, 0.00),
                    c( 0.00,  0.00,  0.00, 1.00, 0.25),
                    c( 0.00,  0.00,  0.00, 0.25, 1.00))

obj1 <- NormalPredictorsGenerator$new(num_observations = 10000,
                                      num_predictors = 5,
                                      norm_rand_var_sd = 2,
                                      covariance_matrix = cov_mat_5,
                                      predictor_names = sapply(1:5, function(i){paste0("V", i)}))

test_that("getters", {
  expect_equal(obj1$get_num_predictors(), 5)
  expect_equal(obj1$get_num_observations(), 10000)
})

test_that("simulate_predictors", {
  out1 <- obj1$simulate_predictors()
  expect_equal(dim(out1), c(10000, 5))
  expect_equal(unname(cov(out1)), cov_mat_5, tolerance = 0.1)
})
