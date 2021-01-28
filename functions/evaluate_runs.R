evaluate_runs <- function(first_run,subsequent_runs,n){
  future_map_dfr(1:n,function(x){
    e <- list2env(list())
    append(first_run,subsequent_runs) %>%  map(evaluate_run, my_env = e) %>% 
      keep(is.data.frame) %>% bind_rows()
  },.options = future_options(seed = TRUE))
}
