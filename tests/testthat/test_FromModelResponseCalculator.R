context("FromModelResponseCalculator")

x <- 10 * runif(10)
y <- 10 * runif(10)
z <- 2 * x + 7 * y + 15
t1 <- data.frame(x, y, z)
mod_obj1 <- lm(z ~ x + y, data = t1)
err_gen_1 <- function(x) rnorm(n = 1, sd = 2)

obj1 <- FromModelResponseCalculator$new(object_model = mod_obj1,
                                      irreducible_error_generator = err_gen,
                                      response_is_continuous = TRUE)

x1 <- rnorm(1000)
x2 <- rnorm(1000)
z <- 1 + 2*x1 + 3*x2
pr <- 1/(1+exp(-z))
y <- rbinom(1000,1,pr)
mod_obj2 <- glm(y ~ x1 + x2, data = data.frame(x1, x2, y), family = "binomial")
mod_obj2$coefficients <- c(`(Intercept)` = 1, x1 = 2, x2 = 3)

response <- log(prob / (1 - prob))
t2 <- data.frame(x1, x2, response)
mod_obj2 <- glm(response ~ x1 + x2, family = binomial)
err_gen_2 <- function(x)

test_that("response_is_continuous", {
  expect_equals(obj1$response_is_continuous, TRUE)
})

test_that("simulate_predictors", {

})
