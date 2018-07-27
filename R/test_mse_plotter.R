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

#' Function for plotting test mse
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
#' @export
test_mse_boxplot = function(Simulation) {
  rowid <- . <- value <- median_mse <- mean_mse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  x <- tibble::as.tibble(Simulation$get_test_mse()) %>%
    tibble::rowid_to_column(.) %>%
    tidyr::gather(key = "rowid")

  ggplot2::ggplot(data = x, aes(x = rowid, y = value)) +
    geom_boxplot() +
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

#' Function for plotting training mse
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
#' @export
training_mse_boxplot = function(Simulation) {
  rowid <- . <- value <- median_mse <- mean_mse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  x <- as.tibble(Simulation$get_training_mse()) %>%
    rowid_to_column(.) %>%
    tidyr::gather(data = ., key = "rowid")

  ggplot2::ggplot(data = x, aes(x = rowid, y = value)) +
    geom_boxplot() +
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

  sapply(c(Min = min, Max = max, Mean = mean, Median = stats::median), function(f) {
    apply(Simulation$get_test_mse(), 2, f)
  }) %>%
    as.data.frame(., row.names = colnames(Simulation$get_test_mse())) %>%
    knitr::kable(., digits = 3, caption = "Test MSE")
}

#' Function for outputting training mse in a table
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @export
training_mse_tables = function(Simulation) {
  rowid <- . <- value <- median_mse <- mean_mse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  sapply(c(Min = min, Max = max, Mean = mean, Median = stats::median), function(f) {
    apply(Simulation$get_training_mse(), 2, f)
  }) %>%
    as.data.frame(., row.names = colnames(Simulation$get_test_mse())) %>%
    knitr::kable(., digits = 3, caption = "Training MSE")
}
