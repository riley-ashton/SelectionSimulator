##############
# Custom Plots
##############

# Takes a list of Simulator objects then produces a geom_count plot
# of all of the beta values. 
# Fitted beta Values are truncated to the first digit, then counted
betas_plotter <- function(what, nn, pp, qq) {
  x <- what %>% 
    filter(n == nn, p == pp, q == qq) %>%
    .$beta_hat %>% unlist(.) %>%
    matrix(., ncol = pp, byrow = TRUE) %>%
    as_tibble() %>% rowid_to_column() %>%
    tidyr::gather(key, value, -rowid) %>%
    mutate(value = round(value, digits = 1))
  
  plot_title <- paste0("Stepwise Fitted Betas when n = ",
                            nn, "  p = ", pp, "  q = ", qq)
  ind_var_ordering <- gtools::mixedsort(unique(x$key))
  
  x %>%
    ggplot(aes(x = key, y = value, group = key)) +
    xlim(ind_var_ordering) +
    geom_count() +
    labs(title = eval(plot_title))
}

# Takes a list of Simulator objects then produces a heat map
# of all of the beta values
# Unselected values (beta_i = 0) are set to black
betas_heat_map_generic <- function(what, param, nn, pp, qq) {
  x <- what %>% 
    filter(n == nn, p == pp, q == qq) %>%
    .[[param]] %>% unlist(.) %>%
    matrix(., ncol = pp, byrow = TRUE) %>%
    as_tibble() %>% rowid_to_column %>%
    tidyr::gather(key, value, -rowid)
  
  x$value <- sapply(x$value, function(x) ifelse(x==0, NA, x))
  
  x %>%
    ggplot(aes(key, rowid, fill=value)) + geom_raster() + 
    xlim(gtools::mixedsort(unique(x$key))) +
    scale_fill_gradient2(low="red", high="blue", mid = "white",
                         midpoint = 1, na.value = "black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    labs(title = eval(paste0("Stepwise Fitted Betas Heatmap when n = ",
                             nn, "  p = ", pp, "  q = ", qq)))
}
betas_heat_map <- purrr::partial(betas_heat_map_generic,
                                 param = "beta_hat")
betas_heat_map_Step2 <- purrr::partial(betas_heat_map_generic,
                                       param = "beta_hat_Step2")
betas_heat_map_Step3 <- purrr::partial(betas_heat_map_generic,
                                       param = "beta_hat_Step3")

# Takes a list of Simulator objects then produces a heat map
# of the beta values that should have been selected (the q values)
#
# Unselected values (beta_i = 0) are set to black
#
# Useful when q << p and you want to see the fitted values 
# for covariates in the model
betas_heat_map_q_generic <- function(what, param, nn, pp, qq) {
  x <- what %>% 
    filter(n == nn, p == pp, q == qq) %>%
    .[[param]] %>% unlist(.) %>%
    matrix(., ncol = pp, byrow = TRUE) %>%
    as_tibble() %>% .[(pp-qq+1):pp] %>%
    rowid_to_column %>%
    tidyr::gather(key, value, -rowid)
  
  x$value <- sapply(x$value, function(x) ifelse(x==0, NA, x))
  
  plot_title <- paste0("Stepwise Fitted Betas Heatmap when n = ",
                            nn, "  p = ", pp, "  q = ", qq)
  
  x %>%
    ggplot(aes(key, rowid, fill=value)) + geom_raster() + 
    xlim(gtools::mixedsort(unique(x$key))) +
    scale_fill_gradient2(low="red", high="blue", mid = "white",
                         na.value = "black", midpoint = 1) +
    labs(title = eval(plot_title)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0))
}

betas_heat_map_q <- purrr::partial(betas_heat_map_q_generic,
                                   param = "beta_hat")
betas_heat_map_q_Step2 <- purrr::partial(betas_heat_map_q_generic,
                                               param = "beta_hat_Step2")
betas_heat_map_q_Step2 <- purrr::partial(betas_heat_map_q_generic,
                                         param = "beta_hat_Step3")


# Takes a list of Simulator objects then produces a table for 
# the order of inclusion of covariates by the stepwise algorithm
# Also includes the delta_rss and beta_ss values
inclusion2table <- function(what, nn, pp, qq, pdf_head = 15) {
  temp <- what %>% 
    filter(n == nn, p == pp, q == qq) %>% 
    group_by(inclusion_order)
  
  out <- inner_join(
    temp %>% summarise(beta_ss = round(mean(beta_ss), 2), 
                       delta_rss = round(mean(delta_rss), 2)),
    temp %>% count,
    by = "inclusion_order"
  ) %>% 
    arrange(desc(nn))
  
  if(knitr::is_html_output()) {
    table_out <- DT::datatable(out)
  } else {
    table_out <- out %>%
      head(15) %>%
      knitr::kable(.)
  }
  return(table_out)
}

# Takes parameters and a Simulator constructor (ex Simulator2$new),
# and makes a simple simulation that outputs a results table
simple_simulation_generic <- function(file, simulation_size, n, beta_0, betas,
                       sigma_err, k, cov_mat, Simulator_constructor) {
  if(! file.exists(file)) {
    sim_list <- replicate(simulation_size, 
                          Simulator_constructor(n, beta_0, betas, 
                                                sigma_err, k, cov_mat))
    
    sim_list <- parallel::mclapply(sim_list, function(sim) {
      sim$simulate()
      sim$reduce_size()
    }, mc.cores = min(max_cores, parallel::detectCores()))
    
    results <- sim_list2tibble(sim_list)
    saveRDS(results, file = file)
    return(results)
  } else {
    return(readRDS(file))
  }
}
simple_simulation <- purrr::partial(simple_simulation_generic,
                                    Simulator_constructor = Simulator$new)
simple_simulation_Step2 <- purrr::partial(simple_simulation_generic,
                                    Simulator_constructor = Simulator2$new)
simple_simulation_Step3 <- purrr::partial(simple_simulation_generic,
                                    Simulator_constructor = Simulator3$new)


# Takes a sim_list and produces a numerical results table on the estimated
# betas of each Simulator class (base R Step, Step2, Step3)
# sim_list must be a list of Simulator, Simulator2 or Simulator3 objects
# from the same simulation (ie identical n, betas, cov_mat, etc)
numerical_results <- function(sim_list) {
  out <- tibble(Step = 1:3)
  n <- sim_list[[1]]$n
  predictor_count <- length(sim_list[[1]]$betas)
  for(i in (1:predictor_count)) {
    new_col <- c(`base step` = get_predictor_mse(i, sim_list, "step_lm_results"),
                 Step2 = get_predictor_mse(i, sim_list, "Step2_results"),
                 Step3 = get_predictor_mse(i, sim_list, "Step3_results"))
    col_name <- paste0("V", i)
    out <- add_column(out, !! col_name := new_col)
  }
  if(knitr::is_html_output()) {
    return(DT(out))
  } else {
    knitr::kable(out)
  }
}

get_predictor_mse <- function(i, sim_list, param) {
  Reduce(function(sum, sim) {
    sum <- sum + 
      tryCatch((sim[[param]]$coefficients[[paste0("V", i)]] - sim$betas[[i]]) ^ 2,
               error = function(e){sim$betas[[i]] ^ 2})
  }, sim_list, init = 0)
}