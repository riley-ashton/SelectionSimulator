library(dplyr)

# ` @importFrom dplyr %>%

inclusion_order <- function(Simulation, pdf_head = 15) {
  x <- tibble::as.tibble(Simulation$get_inclusion_orders()) %>%
    tidyr::gather()

  x$value <- sapply(x$value, function(z) {
    Reduce(function(x, y) paste(x,y, sep = ", "), z)
  })

  x <- x %>%
    dplyr::group_by(value, key) %>%
    dplyr::count

  if(knitr::is_html_output()) {
    table_out <- DT::datatable(x)
  } else {
    table_out <- x %>%
      utils::head(15) %>%
      knitr::kable(.)
  }
  return(table_out)
}
