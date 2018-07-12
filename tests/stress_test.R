cov_mat_5 <- rbind( c(1.00, -0.80,  0.75, 0.00, 0.00),
                    c(-0.80,  1.00, -0.75, 0.00, 0.00),
                    c( 0.75, -0.75,  1.00, 0.00, 0.00),
                    c( 0.00,  0.00,  0.00, 1.00, 0.25),
                    c( 0.00,  0.00,  0.00, 0.25, 1.00))

cov_mat_100 <- as.matrix(Matrix::bdiag(replicate(20, cov_mat_5, simplify = FALSE)))
predict_gen <- NormalPredictorsGenerator$new(num_observations = 1000,
                                             num_predictors = 100,
                                             norm_rand_var_sd = 2,
                                             covariance_matrix = cov_mat_100)

coefficients_1 <- c(replicate(50, 0), replicate(50, 1))
names(coefficients_1) <- sapply(seq.int(100), function(x) paste0("V", x))

response_calc_1 <- LinearNormalResponseCalculator$new(norm_rand_var_sd = 1,
                                                      coefficients = coefficients_1,
                                                      intercept = 9)

sim_data_gen <- SimulationDataGenerator$new(predict_gen, response_calc_1)

test_obj_1 <- Simulation$new(sim_data_gen, c(Step2 = Step2$new, Step3 = Step3$new), 8, 1)

test_obj_1$simulate()
