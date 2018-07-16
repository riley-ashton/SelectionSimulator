#' Function for outputting inclusion order as a table
#' @param Simulation object
#' @param pdf_head if pdf output how many rows to show, at maximum
#' @import dplyr
#' @import tibble
#' @importFrom tidyr gather
#' @importFrom DT datatable
#' @export
inclusion_order <- function(Simulation, pdf_head = 15) {
  key <- . <- value <- NULL # Fixes  no visible binding for global variable CMD Check error

  x <- tibble::as.tibble(Simulation$get_inclusion_orders()) %>%
    tidyr::gather()

  x$value <- sapply(x$value, function(z) {
    Reduce(function(x, y) paste(x,y, sep = ", "), z)
  })

  x <- x %>%
    dplyr::group_by(value, key) %>%
    dplyr::count(.) %>%
    dplyr::arrange(desc(n))

  if(knitr::is_html_output()) {
    table_out <- DT::datatable(x)
  } else {
    table_out <- x %>%
      utils::head(pdf_head) %>%
      knitr::kable(.)
  }
  return(table_out)
}
