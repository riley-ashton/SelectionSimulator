betas_heat_map = function(Simulator, indices = TRUE) {
  browser()
  coefficients <- Simulator$get_fitted_coefficients()
  n <- dim(coefficients)[[3]]
  plots = vector(mode = "list", length = n)
  for(i in seq.int(n)) {
    plots[[i]] <- betas_heat_map_helper(coefficients[,,i])
  }
  do.call(gridExtra::grid.arrange,plots)
}

betas_heat_map_helper = function(matrix_slice) {
  x <- as.tibble(matrix_slice) %>%
    rowid_to_column() %>%
    tidyr::gather(key, value, -rowid) %>%
    mutate(value = round(value, digits = 1))

  x$value <- sapply(x$value, function(x) ifelse(x==0, NA, x))

  x %>%
    ggplot(aes(key, rowid, fill=value)) + geom_raster() +
    xlim(gtools::mixedsort(unique(x$key))) +
    scale_fill_gradient2(low="red", high="blue", mid = "white",
                         midpoint = 1, na.value = "black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    labs(title = "TODO")
}
