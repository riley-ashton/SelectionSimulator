#' Function for plotting fitted betas as a heat map
#' @param Simulator Simulator object
#' @param hide_intercept Whether to inlcude the intercept in the heat map
#' @export
betas_heat_map = function(Simulator, hide_intercept = TRUE) {
  coefficients <- Simulator$get_fitted_coefficients()
  n <- dim(coefficients)[[3]]
  plots <- vector(mode = "list", length = n)
  algo_names <- colnames(Simulator$get_inclusion_orders())
  for(i in seq.int(n)) {
    plots[[i]] <- betas_heat_map_helper(coefficients[,,i],
                                        algo_names[[i]], hide_intercept)
  }
  do.call(gridExtra::grid.arrange,plots)
}

#' Helper function for betas_heat_map
#' @param matrix_slice fitted coefficient values from a single stepwise algorithm
#' @param hide_intercept passthrough parameter
#' @return a ggplot of a beta heat map from a single stepwise algorithm
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
betas_heat_map_helper = function(matrix_slice, algo_name, hide_intercept) {
  key <- rowid <- value <- NULL # Fixes  no visible binding for global variable CMD Check error

  x <- tibble::as.tibble(matrix_slice) %>%
    tibble::rowid_to_column() %>%
    tidyr::gather(key, value, -rowid)

  if(hide_intercept) {
    x <- x %>% dplyr::filter(key != "(Intercept)")
  }

  x$value <- sapply(x$value, function(x) ifelse(x==0, NA, x))

  x %>%
    ggplot(ggplot2::aes(key, rowid, fill=value)) +
    geom_raster() +
    xlim(gtools::mixedsort(unique(x$key))) +
    scale_fill_gradient2(low="red", high="blue", mid = "white",
                         midpoint = 1, na.value = "black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    labs(title = algo_name) +
    xlab("Model Coefficients") +
    ylab("Simulation Number")
}
