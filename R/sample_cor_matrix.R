#' Function for displaying generated sample correlation matrix
#' @param Simulator Simulator object
#' @export
sample_cor_matrix  <- function(Simulator) {
  max_cors <- Simulator$get_max_sample_correlation()
  min_cors <- Simulator$get_min_sample_correlation()
  response_name <- Simulator$SimulationDataGenerator$get_response_name()
  response_index <- match(response_name, row.names(max_cors))
  max_cors <- max_cors[-response_index, -response_index]
  min_cors <- min_cors[-response_index, -response_index]

  out <- mapply(function(min, max){
    min_str <- as.character(round(min, 2))
    max_str <- as.character(round(max, 2))
    paste0(paste0(min_str, "/"), max_str)
  }, min_cors, max_cors)
  out <- matrix(data = out, nrow = sqrt(length(out)))
  diag(out) <- "     X   "
  out <- apply(mat, MARGIN = 2, function(x) Reduce(purrr::partial(paste, sep = " & "), x))
  Reduce(purrr::partial(paste, sep = " \\ "), out)
}
