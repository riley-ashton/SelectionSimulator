context("FromModelResponseCalculator")

#######################
# Linear gaussian model
#######################
x <- 10 * runif(1000)
y <- 10 * runif(1000)
z <- 2 * x + 7 * y + 15
t1 <- data.frame(x, y, z)
mod_obj1 <- lm(z ~ x + y, data = t1)
err_gen_1 <- function(x) rnorm(n = 1, sd = 2)

obj1 <- FromModelResponseCalculator$new(object_model = mod_obj1,
                                      irreducible_error_generator = err_gen_1,
                                      response_is_continuous = TRUE)

test_that("response_is_continuous", {
  expect_equal(obj1$response_is_continuous(), TRUE)
})

response <- obj1$calculate_response(data.frame(x, y))

test_that("correct_response_dimensions", {
  expect_equal(length(response), 1000)
})

test_that("simulate_predictors_similar_means", {
  expect_equal(sum(abs(response - z)), 0, tolerance = 1)
})

test_that("simulate_predictors_normality_tests", {
  expect_gt(shapiro.test(response - z)$statistic, 0.99)
  expect_equal(sd(response - z), 2, tolerance = 0.05)
})





################
# Logistic model - Not yet completed
################

x1 <- rnorm(10000)
x2 <- rnorm(10000)
z <- 1 + 2*x1 + 3*x2
prob <- 1/(1+exp(-z))
y <- rbinom(10000,1,prob)
mod_obj2 <- glm(y ~ x1 + x2, data = data.frame(x1, x2, y), family = "binomial")

# err_gen_2 <- function(x)
