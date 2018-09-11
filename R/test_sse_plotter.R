#' Function for plotting test sse
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
#' @export
test_sse_boxplot = function(Simulation) {
  rowid <- . <- value <- median_sse <- mean_sse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  x <- tibble::as.tibble(Simulation$get_test_sse()) %>%
    tibble::rowid_to_column(.) %>%
    tidyr::gather(key = "rowid")

  ggplot2::ggplot(data = x, aes(x = rowid, y = value)) +
    geom_boxplot() +
    labs(title = "Test SSE by Algorithm") +
    ylab("Test SSE Value") +
    xlab("Stepwise Algorithm")
}

#' Function for plotting training sse
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
#' @export
training_sse_boxplot = function(Simulation) {
  rowid <- . <- value <- median_sse <- mean_sse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  x <- as.tibble(Simulation$get_training_sse()) %>%
    rowid_to_column(.) %>%
    tidyr::gather(data = ., key = "rowid")

  ggplot2::ggplot(data = x, aes(x = rowid, y = value)) +
    geom_boxplot() +
    labs(title = "Training SSE by Algorithm") +
    ylab("Training SSE Value") +
    xlab("Stepwise Algorithm")
}

#' Function for outputting test sse in a dataframe
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @export
test_sse2dataframe = function(Simulation) {
  rowid <- . <- value <- median_sse <- mean_sse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  sapply(c(Min = min, Max = max, Mean = mean, Median = stats::median), function(f) {
    apply(Simulation$get_test_sse(), 2, f)
  }) %>%
    as.data.frame(., row.names = colnames(Simulation$get_test_sse()))
}

#' Function for outputting test sse in a knitr kable
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @export
test_sse2kable = function(Simulation) {
  test_sse2dataframe(Simulation) %>%
    knitr::kable(., digits = 3, caption = "Test SSE")
}

#' Function for outputting training sse in a dataframe
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @export
training_sse2dataframe = function(Simulation) {
  rowid <- . <- value <- median_sse <- mean_sse <- NULL # Fixes 'no visible binding for global variable' CMD Check error

  sapply(c(Min = min, Max = max, Mean = mean, Median = stats::median), function(f) {
    apply(Simulation$get_training_sse(), 2, f)
  }) %>%
    as.data.frame(., row.names = colnames(Simulation$get_test_sse()))
}

#' Function for outputting training sse in a knitr kable
#' @param Simulation Simulation object to plot
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @export
training_sse2kable = function(Simulation) {
  training_sse2dataframe(Simulation) %>%
    knitr::kable(., digits = 3, caption = "Training SSE")
}
