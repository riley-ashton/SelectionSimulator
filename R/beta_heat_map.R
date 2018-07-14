library(dplyr)
library(ggplot2)
# ` @importFrom dplyr %>%

betas_heat_map = function(Simulator, hide_intercept = TRUE) {
  coefficients <- Simulator$get_fitted_coefficients()
  n <- dim(coefficients)[[3]]
  plots = vector(mode = "list", length = n)
  for(i in seq.int(n)) {
    plots[[i]] <- betas_heat_map_helper(coefficients[,,i], hide_intercept)
  }
  do.call(gridExtra::grid.arrange,plots)
}

# ` @importFrom dplyr %>%

betas_heat_map_helper = function(matrix_slice, hide_intercept) {
  x <- tibble::as.tibble(matrix_slice) %>%
    tibble::rowid_to_column() %>%
    tidyr::gather(key, value, -rowid)

  if(hide_intercept) {
    x <- x %>% dplyr::filter(key != "(Intercept)")
  }

  x$value <- sapply(x$value, function(x) ifelse(x==0, NA, x))

  x %>%
    ggplot2::ggplot(ggplot2::aes(key, rowid, fill=value)) +
    ggplot2::geom_raster() +
    ggplot2::xlim(gtools::mixedsort(unique(x$key))) +
    ggplot2::scale_fill_gradient2(low="red", high="blue", mid = "white",
                         midpoint = 1, na.value = "black") +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    ggplot2::labs(title = "TODO")
}
