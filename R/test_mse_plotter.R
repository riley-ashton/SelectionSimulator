library(dplyr)
library(ggplot2)

#` @importFrom dplyr %>%

test_mse_plotter = function(Simulation) {

  x <- tibble::as.tibble(Simulation$get_test_mse()) %>%
    tibble::rowid_to_column %>%
    tidyr::gather(key = "rowid")

  ggplot2::ggplot(data = x, aes(x = rowid, y = value)) +
    geom_violin() +
    labs(title = "Test MSE by Algorithm") +
    ylab("Test MSE Value") +
    xlab("Stepwise Algorithm")
}

training_mse_plotter = function(Simulation) {

  x <- as.tibble(Simulation$get_training_mse()) %>%
    rowid_to_column %>%
    tidyr::gather(key = "rowid")

  ggplot2::ggplot(data = x, aes(x = rowid, y = value)) +
    geom_violin() +
    labs(title = "Training MSE by Algorithm") +
    ylab("Training MSE Value") +
    xlab("Stepwise Algorithm")
}

# ` @importFrom dplyr %>%

test_mse_tables = function(Simulation) {
  sapply(c(min_mse = min, max_mse = max, median_mse = stats::median, mean_mse = mean), function(f) {
    apply(Simulation$get_test_mse(), 2, f)
  }) %>% as_tibble(.) %>%
    dplyr::mutate(relative_median_mse = median_mse / max(median_mse),
           relative_mean_mse = mean_mse / max(mean_mse))
}

# ` @importFrom dplyr %>%

training_mse_tables = function(Simulation) {
  sapply(c(min_mse = min, max_mse = max, median_mse = stats::median, mean_mse = mean), function(f) {
    apply(Simulation$get_training_mse(), 2, f)
  }) %>% tibble::as_tibble(.) %>%
    dplyr::mutate(relative_median_mse = median_mse / max(median_mse),
           relative_mean_mse = mean_mse / max(mean_mse))
}
