#' Function for plotting test mse
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
#' @export
test_mse_plotter = function(Simulation) {
  rowid <- . <- value <- median_mse <- mean_mse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  x <- tibble::as.tibble(Simulation$get_test_mse()) %>%
    tibble::rowid_to_column(.) %>%
    tidyr::gather(key = "rowid")

  ggplot2::ggplot(data = x, aes(x = rowid, y = value)) +
    geom_violin() +
    labs(title = "Test MSE by Algorithm") +
    ylab("Test MSE Value") +
    xlab("Stepwise Algorithm")
}

#' Function for plotting training mse
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
#' @export
training_mse_plotter = function(Simulation) {
  rowid <- . <- value <- median_mse <- mean_mse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  x <- as.tibble(Simulation$get_training_mse()) %>%
    rowid_to_column(.) %>%
    tidyr::gather(data = ., key = "rowid")

  ggplot2::ggplot(data = x, aes(x = rowid, y = value)) +
    geom_violin() +
    labs(title = "Training MSE by Algorithm") +
    ylab("Training MSE Value") +
    xlab("Stepwise Algorithm")
}

#' Function for outputting test mse in a table
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @export
test_mse_tables = function(Simulation) {
  rowid <- . <- value <- median_mse <- mean_mse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  sapply(c(min_mse = min, max_mse = max, median_mse = stats::median, mean_mse = mean), function(f) {
    apply(Simulation$get_test_mse(), 2, f)
  }) %>% as_tibble(.) %>%
    dplyr::mutate(relative_median_mse = median_mse / max(median_mse),
           relative_mean_mse = mean_mse / max(mean_mse))
}

#' Function for outputting training mse in a table
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @export
training_mse_tables = function(Simulation) {
  rowid <- . <- value <- median_mse <- mean_mse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  sapply(c(min_mse = min, max_mse = max, median_mse = stats::median, mean_mse = mean), function(f) {
    apply(Simulation$get_training_mse(), 2, f)
  }) %>% tibble::as_tibble(.) %>%
    dplyr::mutate(relative_median_mse = median_mse / max(median_mse),
           relative_mean_mse = mean_mse / max(mean_mse))
}
