#' Function for outputting inclusion order as a table
#' @param Simulation object
#' @param pdf_head if pdf output how many rows to show, at maximum
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
#' @export
inclusion_order <- function(Simulation, pdf_head = 10) {
  key <- . <- value <- NULL # Fixes  no visible binding for global variable CMD Check error

  x <- tibble::as.tibble(Simulation$get_inclusion_orders())

  algo_names <- colnames(x)
  title <- paste0(
    paste0("Inclusion Order for ", Reduce(function(x, y) paste(x, y, sep = ", "), algo_names),
           " Respectively"))

  for(y in algo_names){
    x[[y]] <- sapply(x[[y]], function(z) {
      if(length(z) == 0) {
        init <- ""
      } else {
        init <- Reduce(function(x, y) paste(x,y, sep = " | "), z)
      }
      tail <- " ||"
      paste0(init, tail)
    })
  }

  out <- vector(mode = "list", length = length(algo_names))


  for(i in seq_along(algo_names)) {
    y <- algo_names[[i]]
    temp <- as.tibble(x[[y]]) %>%
      group_by_all(.) %>%
      count %>%
      arrange(desc(n)) %>%
      head(pdf_head)
    colnames(temp) <- c(paste0("Order ", y), paste0("# " , y))

    out[[i]] <- temp
  }
  knitr::kable(out, align='c', caption = title)
}
