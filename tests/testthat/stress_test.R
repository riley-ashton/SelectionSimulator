cov_mat_5 <- rbind( c(1.00, -0.80,  0.25, 0.00, 0.00),
                    c(-0.80,  1.00, -0.75, 0.00, 0.00),
                    c( 0.25, -0.75,  1.00, 0.00, 0.00),
                    c( 0.00,  0.00,  0.00, 1.00, -0.75),
                    c( 0.00,  0.00,  0.00, -0.75, 1.00))

coefficients_1 <- c(replicate(5, 0), replicate(5, 1))

cov_mat_10 <- as.matrix(Matrix::bdiag(replicate(2, cov_mat_5, simplify = FALSE)))
predict_gen <- NormalPredictorsGenerator$new(num_observations = 100,
                                             num_predictors = 10,
                                             norm_rand_var_sd = 1,
                                             covariance_matrix = cov_mat_10)

names(coefficients_1) <- sapply(seq.int(10), function(x) paste0("V", x))

response_calc_1 <- LinearNormalResponseCalculator$new(norm_rand_var_sd = 1,
                                                      coefficients = coefficients_1,
                                                      intercept = 9)

sim_data_gen <- SimulationDataGenerator$new(predict_gen, response_calc_1)


constructors <- c(Step = StepLmWrapper$new, Step2 = Step2$new, Step3 = Step3$new)

test_obj_1 <- Simulation$new(sim_data_gen, constructors, 250, 1)

test_obj_1$simulate()

# Test graphics
betas_heat_map(test_obj_1)
inclusion_order(test_obj_1)
test_mse_plotter(test_obj_1)
training_mse_plotter(test_obj_1)
test_mse_tables(test_obj_1)
training_mse_tables(test_obj_1)
